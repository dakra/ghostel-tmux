;;; ghostel-tmux.el --- Tmux control mode for ghostel -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/ghostel-tmux
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ghostel "0.62.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Connects to an external tmux session via control mode (`tmux -CC') and
;; materializes its panes into native ghostel buffers.  Each tmux pane is
;; rendered through a real ghostel terminal instance, so all of ghostel's
;; navigation, scrollback, copy-mode, theme-sync and evil integration apply
;; unchanged.
;;
;; Entry points:
;;
;;   M-x ghostel-tmux-attach   -- attach to a running tmux session, materialize
;;                                all panes, and route input/output live.
;;   M-x ghostel-tmux-new      -- create or attach to a session by name.
;;   M-x ghostel-tmux-capture  -- one-shot snapshot of a single pane's
;;                                scrollback into a fresh buffer; no live
;;                                connection is established.
;;   ghostel-tmux-takeover-mode -- `tmux -CC' typed in a ghostel shell,
;;                                also over ssh, turns into pane buffers.
;;
;; The protocol layer is implemented purely in Elisp.  For the takeover,
;; ghostel reports the control-mode DCS in a shell's output through
;; `ghostel-control-mode-function'.  Pane data is fed into the existing
;; native VT engine via `ghostel--write-vt', and outbound keystrokes are
;; routed through `ghostel-pty-out-function' to the control-mode channel
;; as `send-keys -H' commands.

;;; Code:

(require 'ghostel)
(require 'cl-lib)
(require 'subr-x)
(require 'tab-line)

(declare-function ghostel--write-vt "ghostel-module" (term data))
(declare-function ghostel--write-pty "ghostel-module" (term data))


;; Customization

(defgroup ghostel-tmux nil
  "Tmux control mode integration for ghostel."
  :group 'ghostel
  :prefix "ghostel-tmux-")

(defcustom ghostel-tmux-program "tmux"
  "The tmux executable to invoke for control mode.
With a remote `default-directory', it names a program on that host."
  :type 'string
  :group 'ghostel-tmux)

(defcustom ghostel-tmux-prefix-key "C-t"
  "Key sequence used as the tmux prefix in pane buffers.
A `kbd'-style string (e.g. \"\\`C-t'\", \"\\`C-b'\").  Changing this
rebinds the prefix in `ghostel-tmux-pane-mode-map', so existing pane
buffers pick up the change immediately."
  :type 'string
  :group 'ghostel-tmux
  :set (lambda (sym val)
         (let ((old (and (boundp sym) (symbol-value sym))))
           (set-default sym val)
           (when (boundp 'ghostel-tmux-pane-mode-map)
             (when old
               (define-key ghostel-tmux-pane-mode-map (kbd old) nil))
             (ghostel-tmux--install-prefix-key val)))))

(defcustom ghostel-tmux-default-rows 40
  "Default row count when creating pane terminals before resize."
  :type 'integer
  :group 'ghostel-tmux)

(defcustom ghostel-tmux-default-cols 120
  "Default column count when creating pane terminals before resize."
  :type 'integer
  :group 'ghostel-tmux)

(defcustom ghostel-tmux-history-lines 10000
  "Maximum scrollback lines requested via `capture-pane' on attach."
  :type 'integer
  :group 'ghostel-tmux)

(defcustom ghostel-tmux-buffer-name-function
  #'ghostel-tmux--default-buffer-name
  "Function returning a pane buffer name.
Called with three arguments: SESSION-NAME (string), WINDOW-NAME (string
or nil) and PANE-ID (integer).  Must return a string."
  :type 'function
  :group 'ghostel-tmux)


;; Internal state — controller buffer

;; The "controller" holds all parser state and dispatches notifications to
;; pane buffers: a hidden buffer owning a tmux -CC subprocess, or the host
;; buffer of a takeover.

(defvar-local ghostel-tmux--controller-process nil
  "The tmux -CC subprocess hosted in this controller buffer.")

(defvar-local ghostel-tmux--parser-state 'preamble
  "Wire-format parser state.
One of: preamble, idle, block.")

(defvar-local ghostel-tmux--line-buffer ""
  "Partial line accumulator across `process-filter' chunks.")

(defvar-local ghostel-tmux--block-lines nil
  "Reversed list of lines accumulated for the in-flight block payload.")

(defvar-local ghostel-tmux--block-tag nil
  "When inside a `%begin'/`%end' block, its plist of :time, :cmd and :flags.")

(defvar-local ghostel-tmux--command-queue nil
  "FIFO of pending tmux commands, each (CMD-STRING . CONTINUATION).
CONTINUATION is called with one argument BLOCK-OUTPUT (string) when the
matching `%end' arrives; or with nil when an `%error' is received.")

(defvar-local ghostel-tmux--command-in-flight nil
  "The (CMD-STRING . CONTINUATION) currently awaiting a block reply.")

(defvar-local ghostel-tmux--reply-error nil
  "The text of tmux's error reply to the last command, or nil.")

(defvar-local ghostel-tmux--session-id nil
  "Numeric tmux session id this controller is attached to.")

(defvar-local ghostel-tmux--session-name nil
  "Human-readable tmux session name.")

(defvar-local ghostel-tmux--panes nil
  "Hash table mapping integer pane-id (e.g. %3 -> 3) to its buffer.")

(defvar-local ghostel-tmux--windows nil
  "Hash table mapping integer window-id to a plist.
Its keys are :name, :panes, :active-pane (tmux's active pane of that
window) and :zooming (see `ghostel-tmux--ensure-pane-zoomed').")

(defvar-local ghostel-tmux--window-order nil
  "List of integer window-ids in tmux's order, each once.")

(defvar-local ghostel-tmux--current-window-id nil
  "Window id of the session's current window in tmux, or nil before known.")

(defvar-local ghostel-tmux--active-pane-id nil
  "Pane id of the active pane of the session's current window, or nil.
While tmux's active pane is floating and has no buffer, this can be the
tiled pane Emacs shows in its place.")

(defvar-local ghostel-tmux--pending-follow-wid nil
  "Window id whose active pane we should follow once it materializes.
Set by `--on-session-window-changed' when tmux moves the session
to a window we haven't yet populated (typical for `new-window':
`%session-window-changed' fires before `%window-add'+list-windows
materialize the pane buffers), or whose active pane is not among its
panes.  Consumed by `--on-layout-change', dropped by `--select-pane'.")

(defvar-local ghostel-tmux--exit-reason nil
  "Why tmux is leaving, from `%exit', a failed startup or text before the frame.")

(defvar-local ghostel-tmux--closing nil
  "Non-nil once tmux is leaving control mode.
A command sent after `detach-client' or `%exit' would reach the host
shell of a takeover as typed input.")

(defvar-local ghostel-tmux--auto-original-name nil
  "Original buffer name of an auto-takeover host, saved on entry.
In control mode a leading space hides the host from `switch-to-buffer'
completion.  Restored on exit.")

(defvar-local ghostel-tmux--host-identity nil
  "The `ghostel-identity' of an auto-takeover host, saved on entry.
The host has none in control mode, so that `ghostel' does not pick it.")

(defvar ghostel-tmux--in-write-vt nil
  "Non-nil while feeding tmux `%output' bytes into a pane's VT engine.
The engine's replies to queries in the output (OSC colors, DA, cursor
position) are dropped while this is set: through `send-keys' they would
reach the pane after the program asking has usually exited, and land in
the next prompt as if typed.")

(defvar-local ghostel-tmux--auto-host-window nil
  "When non-nil, the window to show tmux's active pane in once it is known.
Set when a takeover starts, for the pane to replace the host's view, on
a switch to another session, whose panes replace the ones shown, when
the active pane or the one standing in for it goes away, and when
`ghostel-tmux--wait-for-pane' gives up or attaches to a current window of
floating panes only.")

(defvar-local ghostel-tmux--dcs-active nil
  "Non-nil when this buffer is currently inside a tmux DCS frame.
Never set in a pane buffer or a dedicated controller: see
`ghostel-tmux--dcs-enter'.")


;; Internal state — pane buffer

(defvar-local ghostel-tmux--controller-buffer nil
  "Pointer back to the controller buffer hosting the tmux process.")

(defvar-local ghostel-tmux--pane-id nil
  "Integer tmux pane id (e.g. 3 for %3) for this buffer.")

(defvar-local ghostel-tmux--window-id nil
  "Integer tmux window id this pane belongs to.")

(defvar-local ghostel-tmux--initialized nil
  "Non-nil once the initial capture-pane backfill has completed.
%output notifications received before this flag is set are dropped:
their effect is already contained in the capture snapshot.")

(defvar-local ghostel-tmux--bootstrap-pending nil
  "Non-nil while this pane's capture-pane backfill is queued.
A second `%layout-change' before the replies must not queue another
pair: that would render the scrollback twice.")


;; Octal escape decoding

;; tmux's `%output' payload escapes 0x00-0x1F and backslash as 3-digit
;; octal "\NNN" — for example ESC arrives as the four ASCII bytes "\033",
;; CR as "\015", LF as "\012".  We must decode these back to raw bytes
;; before feeding them to the VT engine.

(defconst ghostel-tmux--octal-rx
  "\\\\\\(\\\\\\|[0-7][0-7][0-7]\\)"
  "Match a tmux wire escape: 3-digit octal `\\NNN' or `\\\\' (capture-pane -C).")

(defun ghostel-tmux--decode-output (data)
  "Return DATA with tmux wire escapes decoded to raw bytes.
The result is a unibyte string suitable for `ghostel--write-vt'.
Escapes >= \\200 must decode via `unibyte-string': `string' would
produce a multibyte Latin-1 character whose binary encoding is its
two-byte internal form, not the single raw byte."
  (let ((decoded
         (replace-regexp-in-string
          ghostel-tmux--octal-rx
          (lambda (m)
            (let ((esc (match-string 1 m)))
              (if (equal esc "\\") "\\"
                (unibyte-string (string-to-number esc 8)))))
          data t t)))
    (encode-coding-string decoded 'binary)))


;; Layout string parser

;; Layout grammar (ghostty/src/terminal/tmux/layout.zig):
;;
;;   layout    := CHECKSUM ',' node
;;   node      := WIDTHxHEIGHT,X,Y(content)
;;   content   := ',' PANE_ID                      ; leaf
;;              | '{' node (',' node)* '}'         ; horizontal split
;;              | '[' node (',' node)* ']'         ; vertical split
;;
;; We don't model splits visually (single window, tab-line for switching)
;; so we only extract a flat list of (PANE-ID . (W H X Y)) leaves.

(defun ghostel-tmux--parse-layout (layout)
  "Parse a tmux LAYOUT string and return a list of pane records.
Each record is (PANE-ID WIDTH HEIGHT X Y).  The leading 4-char checksum
and its comma are ignored."
  (when (and layout (> (length layout) 5))
    (let ((idx 5)                         ; skip "XXXX,"
          (panes nil))
      (cl-labels
          ((expect (ch)
             (unless (and (< idx (length layout))
                          (eq (aref layout idx) ch))
               (error "Tmux layout: expected %c at %d in %s" ch idx layout))
             (cl-incf idx))
           (read-int ()
             (let ((start idx))
               (while (and (< idx (length layout))
                           (let ((c (aref layout idx)))
                             (and (>= c ?0) (<= c ?9))))
                 (cl-incf idx))
               (when (= start idx)
                 (error "Tmux layout: expected integer at %d in %s"
                        idx layout))
               (string-to-number (substring layout start idx))))
           (parse-node ()
             (let* ((w (read-int))
                    (_ (expect ?x))
                    (h (read-int))
                    (_ (expect ?,))
                    (x (read-int))
                    (_ (expect ?,))
                    (y (read-int))
                    (next (and (< idx (length layout)) (aref layout idx))))
               (cond
                ((eq next ?,)
                 (cl-incf idx)
                 (let ((pid (read-int)))
                   (push (list pid w h x y) panes)))
                ((or (eq next ?\{) (eq next ?\[))
                 (let ((closer (if (eq next ?\{) ?\} ?\])))
                   (cl-incf idx)
                   (parse-node)
                   (while (and (< idx (length layout))
                               (eq (aref layout idx) ?,))
                     (cl-incf idx)
                     (parse-node))
                   (expect closer)))
                (t
                 (error "Tmux layout: unexpected %s at %d in %s"
                        (and next (string next)) idx layout))))))
        (parse-node)
        (nreverse panes)))))


;; Wire-format parser

;; tmux -CC emits a stream framed line-by-line.  States:
;;
;;   preamble     -- before the DCS `\eP1000p'.  Drop bytes until we see it
;;                   (or until we see the first `%' if the DCS was lost).
;;   idle         -- between notifications.  Protocol lines begin with `%'
;;                   (or `\e\\' to end DCS); bare lines are dropped — tmux
;;                   delivers async `run-shell' output as bare lines outside
;;                   any block.
;;   block        -- between `%begin' and `%end'/`%error'.  Lines are
;;                   accumulated verbatim until a terminator line matches.

(defconst ghostel-tmux--block-terminator-rx
  "\\`%\\(end\\|error\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)\\'"
  "Pattern matching a `%end TIME CMD FLAGS' or `%error ...' terminator.")

(defvar ghostel-tmux--in-feed nil
  "Non-nil while `ghostel-tmux--feed-bytes' is dispatching protocol lines.
Controller sends triggered from inside the feed (command
continuations, layout-driven resizes) are deferred to a timer by
`ghostel-tmux--pump-command-queue': a blocking process write can
re-enter process filters and deliver protocol bytes out of order.")

(defun ghostel-tmux--feed-bytes (string)
  "Feed STRING (process output) to the parser in the current buffer.
Splits on newlines, dispatches per-line via the state machine."
  (setq ghostel-tmux--line-buffer
        (concat ghostel-tmux--line-buffer string))
  ;; Strip the DCS preamble `\eP1000p' if we haven't seen it yet.
  (when (eq ghostel-tmux--parser-state 'preamble)
    (let ((p (string-match-p "\eP1000p" ghostel-tmux--line-buffer)))
      (when p
        (setq ghostel-tmux--line-buffer
              (substring ghostel-tmux--line-buffer (+ p 7)))
        (setq ghostel-tmux--parser-state 'idle))))
  (when (eq ghostel-tmux--parser-state 'preamble)
    ;; Some tmux configurations may not send the DCS; if we ever see a
    ;; `%' at line-start, switch to idle.  Anything before that `%' is
    ;; dispatched as idle lines, where non-protocol lines are dropped.
    (when (string-match-p "^%" ghostel-tmux--line-buffer)
      (setq ghostel-tmux--parser-state 'idle)))
  (when (memq ghostel-tmux--parser-state '(idle block))
    (let ((buffer (current-buffer))
          (idx 0)
          (ghostel-tmux--in-feed t))
      ;; `ghostel-tmux--handle-line' may reset
      ;; `ghostel-tmux--line-buffer' under our feet — re-check the
      ;; buffer length before every match so a stale IDX can't index
      ;; past the emptied accumulator.  It may also kill BUFFER: the
      ;; buffer current then can be another controller.
      (unwind-protect
          (while (and (buffer-live-p buffer)
                      (<= idx (length ghostel-tmux--line-buffer))
                      (string-match "\r*\n" ghostel-tmux--line-buffer idx))
            (let ((line (substring ghostel-tmux--line-buffer
                                   idx (match-beginning 0))))
              (setq idx (match-end 0))
              ;; A line that fails must not stop the ones after it.
              (condition-case err
                  (ghostel-tmux--handle-line line)
                (error
                 (message "Ghostel-tmux: Error handling %S: %s"
                          (truncate-string-to-width line 60)
                          (error-message-string err))))))
        (when (buffer-live-p buffer)
          (setq ghostel-tmux--line-buffer
                (if (<= idx (length ghostel-tmux--line-buffer))
                    (substring ghostel-tmux--line-buffer idx)
                  "")))))))

(defun ghostel-tmux--decode-text (text)
  "Return TEXT from the wire, which is UTF-8 bytes, as text."
  (decode-coding-string text 'utf-8))

(defun ghostel-tmux--handle-line (line)
  "Dispatch a single LINE (no trailing CRLF) through the state machine."
  (pcase ghostel-tmux--parser-state
    ('idle (ghostel-tmux--handle-idle-line line))
    ('block (ghostel-tmux--handle-block-line line))
    (_ nil)))

(defun ghostel-tmux--handle-idle-line (line)
  "Parse a single notification LINE while in idle state."
  (cond
   ((string-empty-p line) nil)
   ((string-prefix-p "\e\\" line) nil)   ; DCS ST closer; ignore
   ((not (eq (aref line 0) ?%))
    ;; tmux emits async `run-shell' output as bare lines outside any
    ;; block; drop them.
    nil)
   (t
    (cond
     ((string-match
       "\\`%begin \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)\\'"
       line)
      (setq ghostel-tmux--parser-state 'block
            ghostel-tmux--block-lines nil
            ghostel-tmux--block-tag
            (list :time (match-string 1 line)
                  :cmd (match-string 2 line)
                  :flags (match-string 3 line))))
     (t (ghostel-tmux--dispatch-notification line))))))

(defun ghostel-tmux--handle-block-line (line)
  "Inside a block, check LINE against the terminator and otherwise accumulate.
A terminator only counts when its TIME and CMD fields match the
opening `%begin' tag — block payload can legitimately contain lines
that look like `%end N N N' (e.g. a captured pane that was itself
showing a control-mode transcript)."
  (if (and (string-match ghostel-tmux--block-terminator-rx line)
           (let ((tag ghostel-tmux--block-tag))
             (or (null tag)
                 (and (equal (match-string 2 line) (plist-get tag :time))
                      (equal (match-string 3 line) (plist-get tag :cmd))))))
      (let* ((kind (match-string 1 line))
             (payload (mapconcat #'identity
                                 (nreverse ghostel-tmux--block-lines)
                                 "\n"))
             ;; Flags 0 mark a block of tmux's own, such as a hook's.
             (own (not (equal (plist-get ghostel-tmux--block-tag :flags) "0")))
             (cmd (and own ghostel-tmux--command-in-flight)))
        (setq ghostel-tmux--block-lines nil
              ghostel-tmux--block-tag nil
              ghostel-tmux--parser-state 'idle)
        (when own
          (setq ghostel-tmux--command-in-flight nil))
        ;; A startup command that fails after the attach ends nothing.
        (when (and (null cmd)
                   (null ghostel-tmux--session-id)
                   (equal kind "error"))
          (setq ghostel-tmux--exit-reason
                (ghostel-tmux--decode-text (string-trim payload))))
        (setq ghostel-tmux--reply-error
              (and (equal kind "error")
                   (ghostel-tmux--decode-text (string-trim payload))))
        (when cmd
          (let ((cont (cdr cmd)))
            (when cont
              (condition-case err
                  (funcall cont (and (equal kind "end") payload))
                (error
                 (message "Ghostel-tmux: Command continuation error: %s"
                          (error-message-string err)))))))
        (ghostel-tmux--pump-command-queue))
    (push line ghostel-tmux--block-lines)))

(defun ghostel-tmux--seal-pane-buffer (pane-buf)
  "Seal PANE-BUF as a read-only snapshot, cut off from its controller.
A takeover host may attach again later; the snapshot must not act on
that session."
  (when (buffer-live-p pane-buf)
    (with-current-buffer pane-buf
      (setq ghostel-pty-out-function nil
            ghostel-resize-function nil
            ghostel-tmux--controller-buffer nil
            buffer-read-only t)
      ;; A snapshot has nowhere to send focus or mouse reports, and with
      ;; mouse tracking on, the wheel and clicks would go to them instead
      ;; of scrolling and selecting.
      (when ghostel--term
        (ghostel--write-vt ghostel--term
                           "\e[?1000l\e[?1002l\e[?1003l\e[?1004l"))
      (ghostel-tmux--drop-tab-line))))

(defun ghostel-tmux--seal-panes (controller)
  "Seal every pane buffer belonging to CONTROLLER read-only.
CONTROLLER may already be a killed buffer; its pane table is gone
then, so panes are found by scanning `buffer-list' for pane buffers
whose back-pointer matches."
  (if (buffer-live-p controller)
      (with-current-buffer controller
        (when ghostel-tmux--panes
          (maphash (lambda (_pid buf) (ghostel-tmux--seal-pane-buffer buf))
                   ghostel-tmux--panes)))
    (dolist (buf (buffer-list))
      (when (and (buffer-local-value 'ghostel-tmux-pane-mode buf)
                 (eq (buffer-local-value 'ghostel-tmux--controller-buffer buf)
                     controller))
        (ghostel-tmux--seal-pane-buffer buf)))))

;;;###autoload
(defun ghostel-tmux-reset ()
  "Reset the tmux session this buffer belongs to.
Kills a dedicated `tmux -CC' subprocess or exits control mode in an
auto-takeover host; pane buffers are sealed read-only.  Escape hatch
for a wedged parser."
  (interactive)
  (let ((controller (ghostel-tmux--active-controller)))
    (unless (buffer-live-p controller)
      (user-error "Ghostel-tmux: No active controller for this buffer"))
    (with-current-buffer controller
      (cond
       (ghostel-tmux--dcs-active
        (setq ghostel-tmux--dcs-active nil)
        (ghostel-tmux--exit-control-mode controller)
        ;; An empty line makes tmux leave control mode and close the DCS.
        (when ghostel--term
          (ghostel--write-pty ghostel--term "\n")))
       (t
        (when (and ghostel-tmux--controller-process
                   (process-live-p ghostel-tmux--controller-process))
          (delete-process ghostel-tmux--controller-process))
        (ghostel-tmux--seal-panes controller)
        (message "Ghostel-tmux: Session reset; pane buffers sealed"))))))


;; Notification dispatch

(defun ghostel-tmux--dispatch-notification (line)
  "Dispatch a single notification LINE in idle state."
  (cond
   ((string-match "\\`%output %\\([0-9]+\\) \\(.*\\)\\'" line)
    (let ((pid (string-to-number (match-string 1 line)))
          (data (match-string 2 line)))
      (ghostel-tmux--on-output pid data)))
   ((string-match "\\`%session-changed \\$\\([0-9]+\\) \\(.+\\)\\'" line)
    (ghostel-tmux--on-session-changed
     (string-to-number (match-string 1 line))
     (ghostel-tmux--decode-text (match-string 2 line))))
   ((string-match "\\`%session-window-changed \\$\\([0-9]+\\) @\\([0-9]+\\)\\'"
                  line)
    (ghostel-tmux--on-session-window-changed
     (string-to-number (match-string 1 line))
     (string-to-number (match-string 2 line))))
   ((string-match "\\`%session-renamed \\$\\([0-9]+\\) \\(.+\\)\\'" line)
    (let ((sid (string-to-number (match-string 1 line)))
          (name (ghostel-tmux--decode-text (match-string 2 line))))
      ;; Sent for every session on the server.
      (when (eql sid ghostel-tmux--session-id)
        (ghostel-tmux--on-session-renamed name))))
   ((string-match "\\`%window-add @\\([0-9]+\\)\\'" line)
    (ghostel-tmux--on-window-add
     (string-to-number (match-string 1 line))))
   ((string-match "\\`%window-close @\\([0-9]+\\)\\'" line)
    (ghostel-tmux--on-window-close
     (string-to-number (match-string 1 line))))
   ;; Some tmux versions send this form for a window of this session
   ;; too; either way the session no longer holds it.
   ((string-match "\\`%unlinked-window-close @\\([0-9]+\\)\\'" line)
    (ghostel-tmux--drop-window
     (string-to-number (match-string 1 line))))
   ((string-match "\\`%window-renamed @\\([0-9]+\\) \\(.*\\)\\'" line)
    (ghostel-tmux--on-window-renamed
     (string-to-number (match-string 1 line))
     (ghostel-tmux--decode-text (match-string 2 line))))
   ((string-match
     "\\`%window-pane-changed @\\([0-9]+\\) %\\([0-9]+\\)\\'" line)
    (ghostel-tmux--on-window-pane-changed
     (string-to-number (match-string 1 line))
     (string-to-number (match-string 2 line))))
   ((string-match
     ;;   %layout-change @WID LAYOUT VISIBLE_LAYOUT RAW_FLAGS
     ;; `#{window_raw_flags}' can expand to "" for a window with no
     ;; printable flags, producing a trailing space we must tolerate.
     "\\`%layout-change @\\([0-9]+\\) \\([^ ]+\\)\\(?: \\([^ ]*\\)\\)?\\(?: \\([^ ]*\\)\\)?\\(?: [^ ]*\\)? *\\'"
     line)
    (ghostel-tmux--on-layout-change
     (string-to-number (match-string 1 line))
     (match-string 2 line)
     (match-string 3 line)
     (match-string 4 line)))
   ((string-match "\\`%exit\\(?: \\(.*\\)\\)?\\'" line)
    (ghostel-tmux--on-exit (match-string 1 line)))
   ;; Anything else, such as `%sessions-changed', needs no action.
   (t nil)))


;; Controller output channel

;; A controller comes in two shapes.  A *dedicated* controller (from
;; `ghostel-tmux-attach' / `ghostel-tmux-new') owns a plain `tmux -CC'
;; subprocess whose stdin accepts commands directly.  An *auto-takeover
;; host* rides an existing ghostel shell's PTY: commands must go through
;; `ghostel--write-pty', because with the native PTY backend the
;; buffer's `ghostel--process' is only the event pipe — writing to it
;; goes nowhere.
;;
;; `ghostel-tmux--controller-send' is the low-level transport; every
;; command must reach it through `ghostel-tmux--queue-command'.  tmux
;; control mode emits a `%begin'/`%end' reply block for every command
;; line in wire order, so a command sent outside the FIFO queue while
;; another is in flight shifts the block-to-continuation pairing by one
;; for the rest of the session.

(defun ghostel-tmux--active-controller ()
  "Return the controller of this buffer, which may be one itself, or nil."
  (or ghostel-tmux--controller-buffer
      (and (or (derived-mode-p 'ghostel-tmux-control-mode)
               ghostel-tmux--dcs-active)
           (current-buffer))))

(defun ghostel-tmux--controller-live-p ()
  "Return non-nil when the current controller buffer can reach tmux."
  (if ghostel-tmux--controller-process
      (process-live-p ghostel-tmux--controller-process)
    (and ghostel-tmux--dcs-active ghostel--term t)))

(defun ghostel-tmux--controller-send (data)
  "Send DATA (raw command bytes) to tmux from the current controller buffer."
  (cond
   (ghostel-tmux--controller-process
    (when (process-live-p ghostel-tmux--controller-process)
      (process-send-string ghostel-tmux--controller-process data)))
   ((and ghostel-tmux--dcs-active ghostel--term)
    (ghostel--write-pty ghostel--term data))))


;; Command queue

(defun ghostel-tmux--queue-command (cmd &optional cont)
  "Queue CMD (a tmux command string, no trailing newline) for the controller.
CONT, if given, is called with the block payload (or nil on error).
Commands are dropped once tmux is leaving control mode."
  (unless ghostel-tmux--closing
    (setq ghostel-tmux--command-queue
          (nconc ghostel-tmux--command-queue (list (cons cmd cont))))
    (ghostel-tmux--pump-command-queue)))

(defvar-local ghostel-tmux--pump-scheduled nil
  "Non-nil while a deferred command-queue pump timer is pending.")

(defun ghostel-tmux--pump-command-queue ()
  "Send the next queued command if no command is in flight.
Inside a feed (see `ghostel-tmux--in-feed') the send is deferred to a
timer instead: writing to the controller can block, and a blocking
process write re-enters process filters mid-parse."
  (when (and (not ghostel-tmux--command-in-flight)
             ghostel-tmux--command-queue)
    (if ghostel-tmux--in-feed
        (unless ghostel-tmux--pump-scheduled
          (setq ghostel-tmux--pump-scheduled t)
          (run-at-time 0 nil #'ghostel-tmux--pump-deferred (current-buffer)))
      (when (ghostel-tmux--controller-live-p)
        (let ((cmd (pop ghostel-tmux--command-queue)))
          (setq ghostel-tmux--command-in-flight cmd)
          (ghostel-tmux--controller-send (concat (car cmd) "\n")))))))

(defun ghostel-tmux--pump-deferred (buffer)
  "Run the command-queue pump in controller BUFFER, outside any feed."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ghostel-tmux--pump-scheduled nil)
      (ghostel-tmux--pump-command-queue))))


;; Pane buffer management

(defun ghostel-tmux--default-buffer-name (session-name window-name pane-id)
  "Build a default pane buffer name from SESSION-NAME, WINDOW-NAME, PANE-ID.
A nil WINDOW-NAME (we haven't been told yet) is rendered as
`<unnamed>' so the buffer name doesn't look like a glob pattern."
  (format "*ghostel-tmux:%s:%s%%%d*"
          (or session-name "session")
          (or window-name "<unnamed>")
          pane-id))

(defun ghostel-tmux--get-or-create-pane (controller pane-id window-id
                                                    &optional window-name)
  "Return the pane buffer for PANE-ID under CONTROLLER, creating if needed."
  (with-current-buffer controller
    (let ((existing (gethash pane-id ghostel-tmux--panes)))
      (cond
       ((and existing (buffer-live-p existing))
        (with-current-buffer existing
          (setq ghostel-tmux--window-id window-id))
        existing)
       (t
        (let* ((session-name ghostel-tmux--session-name)
               (name (funcall ghostel-tmux-buffer-name-function
                              (or session-name "?")
                              window-name
                              pane-id))
               (buf (generate-new-buffer name)))
          ;; ghostel refuses a local directory that is gone, such as the
          ;; one an attach started in.
          (unless (or (file-remote-p default-directory)
                      (file-directory-p default-directory))
            (with-current-buffer buf
              (setq default-directory (expand-file-name "~/"))))
          (condition-case err
              (ghostel--init-buffer buf ghostel-tmux-default-rows
                                    ghostel-tmux-default-cols)
            ((error quit)
             (kill-buffer buf)
             (signal (car err) (cdr err))))
          (puthash pane-id buf ghostel-tmux--panes)
          (with-current-buffer buf
            (ghostel-tmux-pane-mode 1)
            (setq ghostel-tmux--controller-buffer controller
                  ghostel-tmux--pane-id pane-id
                  ghostel-tmux--window-id window-id
                  ghostel--term-rows ghostel-tmux-default-rows
                  ghostel--term-cols ghostel-tmux-default-cols
                  ghostel-pty-out-function
                  (apply-partially #'ghostel-tmux--send-bytes-from-pane
                                   controller pane-id)
                  ghostel-resize-function #'ghostel-tmux--resize-pane))
          buf))))))

(defun ghostel-tmux--remove-pane (controller pane-id &optional window-id)
  "Drop pane PANE-ID from CONTROLLER and kill its buffer.
With WINDOW-ID, keep a pane that has moved on to another window."
  (with-current-buffer controller
    (let ((buf (gethash pane-id ghostel-tmux--panes)))
      (when (and buf
                 (or (null window-id)
                     (not (buffer-live-p buf))
                     (eql window-id (buffer-local-value
                                     'ghostel-tmux--window-id buf))))
        (remhash pane-id ghostel-tmux--panes)
        (when (buffer-live-p buf)
          (when (or (eql pane-id ghostel-tmux--active-pane-id)
                    (ghostel-tmux--standing-in-p buf))
            (ghostel-tmux--keep-lost-pane-window buf))
          (with-current-buffer buf
            (setq ghostel-pty-out-function nil))
          (kill-buffer buf))))))

(defun ghostel-tmux--standing-in-p (pane-buf)
  "Return non-nil when PANE-BUF shows the current window for a floating pane.
Such a pane is in the current window, whose active pane has no buffer."
  (and ghostel-tmux--active-pane-id
       ghostel-tmux--current-window-id
       (eql ghostel-tmux--current-window-id
            (buffer-local-value 'ghostel-tmux--window-id pane-buf))
       (not (gethash ghostel-tmux--active-pane-id ghostel-tmux--panes))))

(defun ghostel-tmux--keep-lost-pane-window (pane-buf)
  "Keep the window of PANE-BUF, which is going away.
tmux reports the next active pane after the pane is gone, and sends no
report when its active pane was another one; asking settles both."
  (when-let* ((window (get-buffer-window pane-buf 'visible)))
    (setq ghostel-tmux--auto-host-window window)
    (ghostel-tmux--queue-command "display-message -p '#{pane_id}'"
                                 #'ghostel-tmux--receive-active-pane)))

(defun ghostel-tmux--receive-active-pane (output)
  "Follow the active pane in OUTPUT into a window kept for a lost pane.
A report may have done that already.  A pane without a buffer leaves the
window to the rows, which `--on-layout-change' settles also for a floating
pane; a failed query gives it up."
  (when ghostel-tmux--auto-host-window
    (cond
     ((null output)
      (setq ghostel-tmux--auto-host-window nil))
     ((string-match "%\\([0-9]+\\)" output)
      (let ((pid (string-to-number (match-string 1 output))))
        (if (and ghostel-tmux--panes (gethash pid ghostel-tmux--panes))
            (progn
              (setq ghostel-tmux--active-pane-id pid)
              (ghostel-tmux--follow-active-pane pid))
          (ghostel-tmux--request-window-rows)))))))

(defun ghostel-tmux--pane-list (controller)
  "Return the live pane buffers under CONTROLLER, in window+pane order."
  (with-current-buffer controller
    (delete-dups
     (cl-remove-if-not
      #'buffer-live-p
      (mapcan (lambda (wid)
                (mapcar (lambda (pid) (gethash pid ghostel-tmux--panes))
                        (plist-get (gethash wid ghostel-tmux--windows)
                                   :panes)))
              ghostel-tmux--window-order)))))


;; Notification handlers

(defun ghostel-tmux--on-output (pane-id data)
  "Handle `%output %PANE-ID DATA' in the controller buffer.
Output before the backfill completes is dropped: it is already part of
the capture snapshot."
  (let ((buf (gethash pane-id ghostel-tmux--panes)))
    (when (buffer-live-p buf)
      (let ((decoded (ghostel-tmux--decode-output data)))
        (with-current-buffer buf
          (when ghostel-tmux--initialized
            ;; Native callbacks dispatched while parsing (e.g. OSC 52;e)
            ;; may select another buffer; keep the follow-up
            ;; buffer-local work anchored to this pane buffer.
            (let ((ghostel-tmux--in-write-vt t))
              (save-current-buffer
                (ghostel--write-vt ghostel--term decoded)))
            (ghostel--invalidate)))))))

(defun ghostel-tmux--pane-window ()
  "Return a visible window showing a pane of this controller, or nil.
The selected window comes first."
  (let ((shown (window-buffer))
        window)
    (when ghostel-tmux--panes
      (maphash (lambda (_pid buf)
                 (cond
                  ((eq buf shown) (setq window (selected-window)))
                  ((null window)
                   (setq window (get-buffer-window buf 'visible)))))
               ghostel-tmux--panes))
    window))

(defun ghostel-tmux--on-session-changed (sid name)
  "Handle `%session-changed $SID NAME'."
  (let ((first (null ghostel-tmux--session-id))
        (changed (or (not (equal ghostel-tmux--session-id sid))
                     (not (equal ghostel-tmux--session-name name)))))
    (setq ghostel-tmux--session-id sid
          ghostel-tmux--session-name name)
    (cond
     (first
      ;; Lines typed into a takeover host before this, reached tmux as
      ;; commands: drop their replies and whatever reason they gave.  A
      ;; line typed without RET joins the first line sent, so that line
      ;; is a spare.
      (when ghostel-tmux--dcs-active
        (setq ghostel-tmux--exit-reason nil)
        (ghostel-tmux--queue-until-marker "display-message -p ''" #'ignore))
      (ghostel-tmux--request-window-rows t))
     (changed
      ;; Switching sessions resets pane state.  Wipe and reload; the
      ;; Emacs window goes on showing the new session.
      (setq ghostel-tmux--auto-host-window
            (or (ghostel-tmux--pane-window) ghostel-tmux--auto-host-window))
      (ghostel-tmux--wipe-panes)
      (ghostel-tmux--request-window-rows t)))))

(defconst ghostel-tmux--window-row-format
  "#{window_id}\\t#{pane_id}\\t#{window_active}\\t#{window_name}\\t#{window_layout}\\t#{window_visible_layout}"
  "The `list-windows -F' format that `--receive-window-rows' parses.")

(defun ghostel-tmux--request-window-rows (&optional seed)
  "Ask tmux for the session's windows, which creates any pane missing.
SEED is passed on to `ghostel-tmux--receive-window-rows'."
  (ghostel-tmux--queue-command
   (format "list-windows -F \"%s\"" ghostel-tmux--window-row-format)
   (lambda (output)
     (when output
       (ghostel-tmux--receive-window-rows output seed)))))

(defun ghostel-tmux--on-window-add (wid)
  "Handle `%window-add @WID': fetch its layout and create panes."
  (unless (gethash wid ghostel-tmux--windows)
    (puthash wid (list :name nil) ghostel-tmux--windows)
    (unless (memq wid ghostel-tmux--window-order)
      (setq ghostel-tmux--window-order
            (append ghostel-tmux--window-order (list wid)))))
  (ghostel-tmux--request-window-rows))

(defun ghostel-tmux--on-window-close (wid)
  "Handle `%window-close @WID': drop the window if the session lost it.
tmux also sends this when another session, such as a grouped one, lets
go of a window this session keeps, so ask.  A session that ends sends
`%exit' instead."
  (when (gethash wid ghostel-tmux--windows)
    (ghostel-tmux--queue-command
     "list-windows -F \"#{window_id}\""
     (lambda (output)
       (when (and output
                  (not (member (format "@%d" wid) (split-string output))))
         (ghostel-tmux--drop-window wid))))))

(defun ghostel-tmux--drop-window (wid)
  "Forget window WID and kill its pane buffers."
  (let ((win (gethash wid ghostel-tmux--windows)))
    (when win
      (let ((panes (plist-get win :panes)))
        (dolist (pid panes)
          (ghostel-tmux--remove-pane (current-buffer) pid wid))))
    (remhash wid ghostel-tmux--windows))
  (setq ghostel-tmux--window-order
        (delq wid ghostel-tmux--window-order)))

(defun ghostel-tmux--rename-pane-buffers (window-name pane-ids)
  "Name the buffers of PANE-IDS, in a window called WINDOW-NAME, afresh."
  (let ((session-name ghostel-tmux--session-name))
    (dolist (pid pane-ids)
      (let ((buf (gethash pid ghostel-tmux--panes)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (rename-buffer (funcall ghostel-tmux-buffer-name-function
                                    session-name window-name pid)
                           t)
            (force-mode-line-update)))))))

(defun ghostel-tmux--on-session-renamed (name)
  "Handle `%session-renamed': relabel the session and its pane buffers as NAME."
  (setq ghostel-tmux--session-name name)
  (maphash (lambda (_wid win)
             (ghostel-tmux--rename-pane-buffers (plist-get win :name)
                                                (plist-get win :panes)))
           ghostel-tmux--windows)
  (ghostel-tmux--refresh-tab-line))

(defun ghostel-tmux--on-window-renamed (wid name)
  "Handle `%window-renamed @WID NAME'."
  (when-let* ((win (gethash wid ghostel-tmux--windows)))
    (puthash wid (plist-put win :name name) ghostel-tmux--windows)
    (ghostel-tmux--rename-pane-buffers name (plist-get win :panes)))
  (ghostel-tmux--refresh-tab-line))

(defun ghostel-tmux--follow-active-pane (new-pid)
  "Redirect an Emacs window showing a sibling pane to NEW-PID.
Runs for every switch tmux reports: `%window-pane-changed',
`%session-window-changed', or a deferred re-trigger from
`--on-layout-change' when a report raced ahead of the pane buffer's
creation.  A switch made in Emacs reaches tmux through
`--select-pane', and its report then finds the pane already shown.
A pane some window already shows leaves the other windows alone.
`ghostel-tmux--auto-host-window', the window of a pane that is gone,
comes before any other."
  (let ((new-buf (and new-pid ghostel-tmux--panes
                      (gethash new-pid ghostel-tmux--panes))))
    (when (buffer-live-p new-buf)
      (let (pane-bufs target-window)
        (maphash (lambda (_pid buf) (push buf pane-bufs))
                 ghostel-tmux--panes)
        (walk-windows
         (lambda (window)
           (when (and (not target-window)
                      (memq (window-buffer window) pane-bufs)
                      (not (eq (window-buffer window) new-buf)))
             (setq target-window window)))
         'no-minibuffer 'visible)
        (when (window-live-p ghostel-tmux--auto-host-window)
          (setq target-window ghostel-tmux--auto-host-window))
        (setq ghostel-tmux--auto-host-window nil)
        (when (and target-window
                   (not (get-buffer-window new-buf 'visible)))
          (ghostel-tmux--display-pane-in-window new-buf target-window))))))

(defun ghostel-tmux--on-window-pane-changed (wid pid)
  "Handle `%window-pane-changed @WID %PID': record and follow.
Fires when tmux's active pane changes *within* a window (e.g. `select-pane',
`C-t o', kill-pane auto-survivor pick), in any window of the server.
Each window's active pane is remembered so `--on-session-window-changed' can
restore it on cycle-window; only a change in the session's current window moves
the view.  Switching between windows is signalled by `%session-window-changed'
and handled separately."
  (let ((entry (gethash wid ghostel-tmux--windows)))
    (when entry
      (puthash wid (plist-put entry :active-pane pid)
               ghostel-tmux--windows)))
  (when (eql wid ghostel-tmux--current-window-id)
    (setq ghostel-tmux--active-pane-id pid)
    (ghostel-tmux--follow-active-pane pid)
    ;; Following does nothing for a pane another Emacs window shows, and
    ;; a zoom asked for the pane left would select that pane again.
    (ghostel-tmux--rezoom-shown-pane wid nil)))

(defun ghostel-tmux--on-session-window-changed (sid wid)
  "Handle `%session-window-changed $SID @WID': follow the new window.
Sent for every session on the server; other sessions are ignored.
Follow the window's recorded active pane, or select its first pane when
none is recorded.  A recorded pane that is not in the window and has no
buffer is floating, or moved in by `join-pane' from a window that lost it
first: show the first pane, leave tmux's selection and let the next
`--on-layout-change' settle it.  So does a window without panes yet, or
one whose recorded pane still has its buffer elsewhere."
  (when (eql sid ghostel-tmux--session-id)
    (setq ghostel-tmux--current-window-id wid
          ghostel-tmux--pending-follow-wid nil)
    (let* ((entry (gethash wid ghostel-tmux--windows))
           (panes (and entry (plist-get entry :panes)))
           (recorded (and entry (plist-get entry :active-pane)))
           (known (and recorded (memq recorded panes) recorded))
           (moving (and recorded (not known)
                        (gethash recorded ghostel-tmux--panes)))
           (target (or known (car panes)))
           (buf (and target (gethash target ghostel-tmux--panes))))
      (cond
       ((and (buffer-live-p buf) (not moving))
        (setq ghostel-tmux--active-pane-id target)
        (unless recorded
          (ghostel-tmux--queue-command (format "select-pane -t %%%d" target)))
        (ghostel-tmux--follow-active-pane target)
        (when (and recorded (not known))
          (setq ghostel-tmux--pending-follow-wid wid)))
       (t
        (setq ghostel-tmux--pending-follow-wid wid))))))

(defun ghostel-tmux--on-layout-change (wid layout &optional visible-layout flags)
  "Handle `%layout-change @WID LAYOUT VISIBLE-LAYOUT FLAGS' — reconcile panes.
LAYOUT has every pane of the window, at its unzoomed size in a zoomed
window; VISIBLE-LAYOUT has the panes on screen at their real size.
FLAGS are the window's flags, nil when not known."
  (let* ((panes (ghostel-tmux--parse-layout layout))
         (visible (and visible-layout
                       (not (string-empty-p visible-layout))
                       (ghostel-tmux--parse-layout visible-layout)))
         (pane-ids (mapcar #'car panes))
         (existing (gethash wid ghostel-tmux--windows))
         (old-ids (and existing (plist-get existing :panes)))
         (controller (current-buffer))
         (window-name (and existing (plist-get existing :name))))
    (let ((entry (or existing (list :name window-name))))
      (setq entry (plist-put entry :panes pane-ids))
      (puthash wid entry ghostel-tmux--windows))
    (unless (memq wid ghostel-tmux--window-order)
      (setq ghostel-tmux--window-order
            (append ghostel-tmux--window-order (list wid))))
    (dolist (pid old-ids)
      (unless (memq pid pane-ids)
        (ghostel-tmux--remove-pane controller pid wid)))
    (dolist (rec panes)
      (let* ((pid (nth 0 rec))
             (size (or (assq pid visible) rec))
             (w (nth 1 size))
             (h (nth 2 size))
             (buf (ghostel-tmux--get-or-create-pane
                   controller pid wid window-name)))
        (with-current-buffer buf
          ;; Apply tmux's layout geometry to panes that are NOT visible
          ;; in any Emacs window.  For visible panes, the Emacs window
          ;; size is authoritative (`--apply-pane-size' has already set
          ;; it and sent `refresh-client -C'); applying tmux's layout
          ;; here would fight the zoom-induced full-screen rendering and
          ;; the user would see content at half size after every split.
          (when (and ghostel--term
                     (null (get-buffer-window-list buf nil 'visible))
                     (or (not (eql ghostel--term-cols w))
                         (not (eql ghostel--term-rows h))))
            (ghostel--set-size-with-cell-dims ghostel--term h w)
            (setq ghostel--term-cols w
                  ghostel--term-rows h)
            (ghostel-tmux--commit-hidden-resize))
          (unless (or ghostel-tmux--initialized
                      ghostel-tmux--bootstrap-pending)
            (setq ghostel-tmux--bootstrap-pending t)
            (ghostel-tmux--bootstrap-pane controller pid)))))
    ;; A layout alone does not say which pane is active: wait for the
    ;; window's rows.  An active pane that was not here and has no buffer
    ;; is a floating one, which no layout lists: the first pane stands in.
    (let* ((recorded (plist-get (gethash wid ghostel-tmux--windows)
                                :active-pane))
           (shown (cond
                   ((memq recorded pane-ids) recorded)
                   ((and recorded
                         (not (memq recorded old-ids))
                         (not (gethash recorded ghostel-tmux--panes)))
                    (car pane-ids)))))
      (when shown
        ;; Show tmux's active pane in the window kept for it.
        (when (and (window-live-p ghostel-tmux--auto-host-window)
                   (eql wid ghostel-tmux--current-window-id))
          (let ((pane (gethash shown ghostel-tmux--panes))
                (target-window ghostel-tmux--auto-host-window))
            (setq ghostel-tmux--auto-host-window nil
                  ghostel-tmux--active-pane-id shown)
            (when (buffer-live-p pane)
              (ghostel-tmux--display-pane-in-window pane target-window))))
        ;; Deferred follow: `%session-window-changed' fired for this WID
        ;; before its panes were there (typical for new-window — the
        ;; session-window-changed arrives before window-add + list-windows
        ;; materialize the layout), or while its active pane is not among
        ;; them.
        (when (eql ghostel-tmux--pending-follow-wid wid)
          (setq ghostel-tmux--pending-follow-wid nil
                ghostel-tmux--active-pane-id shown)
          (ghostel-tmux--follow-active-pane shown))))
    ;; Also catch the within-window race: `%window-pane-changed'
    ;; arrived before `%layout-change' created the pane buffer.
    (when (and ghostel-tmux--active-pane-id
               (memq ghostel-tmux--active-pane-id pane-ids))
      (ghostel-tmux--follow-active-pane ghostel-tmux--active-pane-id))
    (ghostel-tmux--rezoom-shown-pane wid flags)
    (ghostel-tmux--refresh-tab-line)))

(defun ghostel-tmux--rezoom-shown-pane (wid flags)
  "Zoom the pane shown of window WID again, unless FLAGS say it is zoomed.
tmux unzooms a window when a pane joins or leaves it, while the pane
shown keeps the size of its Emacs window."
  (let* ((pid ghostel-tmux--active-pane-id)
         (buf (and pid (gethash pid ghostel-tmux--panes))))
    (when (and (eql wid ghostel-tmux--current-window-id)
               (buffer-live-p buf)
               (memq pid (plist-get (gethash wid ghostel-tmux--windows) :panes))
               (not (and flags (string-search "Z" flags)))
               (get-buffer-window buf 'visible))
      (ghostel-tmux--ensure-pane-zoomed wid pid))))

(defun ghostel-tmux--on-exit (reason)
  "Handle `%exit' from tmux, recording REASON if given."
  (setq ghostel-tmux--exit-reason (if (or (null reason) (string-empty-p reason))
                                      ghostel-tmux--exit-reason
                                    reason)
        ghostel-tmux--closing t
        ghostel-tmux--command-queue nil)
  (message "Ghostel-tmux: tmux exited%s" (ghostel-tmux--exit-suffix)))

(defun ghostel-tmux--exit-suffix ()
  "Return the text to add to a message that tmux left, naming the reason."
  (if (and ghostel-tmux--exit-reason
           (not (string-empty-p ghostel-tmux--exit-reason)))
      (format ": %s" ghostel-tmux--exit-reason)
    ""))


;; Bootstrap sequence

(defun ghostel-tmux--parse-window-row (line)
  "Parse `list-windows' LINE, or return nil for any other line.
The result is (WID ACTIVE-PANE CURRENT NAME LAYOUT VISIBLE-LAYOUT).
A client without UTF-8, such as `tmux -CC' typed in a C locale, gets
every tab and non-ASCII character of the row as `_'."
  (when (string-match
         "\\`@\\([0-9]+\\)[\t_]%\\([0-9]+\\)[\t_]\\([01]\\)[\t_]\\(.*\\)[\t_]\\(.+\\)[\t_]\\(.*\\)\\'" line)
    (list (string-to-number (match-string 1 line))
          (string-to-number (match-string 2 line))
          (equal (match-string 3 line) "1")
          (ghostel-tmux--decode-text (match-string 4 line))
          (match-string 5 line)
          (match-string 6 line))))

(defun ghostel-tmux--receive-window-rows (output &optional seed)
  "Parse `list-windows' OUTPUT and create/update windows + panes.
Each row is `@WID<TAB>%ACTIVE-PANE<TAB>CURRENT<TAB>WINDOW-NAME<TAB>LAYOUT
<TAB>VISIBLE-LAYOUT': window names contain spaces.
With SEED, for the reply about the session itself, the rows also give
the session's current window and its active pane, before any pane is
created; later changes arrive as notifications."
  (let ((rows (delq nil (mapcar #'ghostel-tmux--parse-window-row
                                (split-string output "\n" t)))))
    (when (and seed (null ghostel-tmux--current-window-id))
      (when-let* ((row (cl-find-if #'caddr rows)))
        (setq ghostel-tmux--current-window-id (car row)
              ghostel-tmux--active-pane-id (cadr row))))
    (pcase-dolist (`(,wid ,active ,_current ,name ,layout ,visible) rows)
      (let ((entry (plist-put (gethash wid ghostel-tmux--windows)
                              :name name)))
        (puthash wid (plist-put entry :active-pane active)
                 ghostel-tmux--windows))
      (ghostel-tmux--on-layout-change wid layout visible))
    ;; All of the session's windows, in tmux's order; a window linked
    ;; twice has two rows.
    (when rows
      (setq ghostel-tmux--window-order (delete-dups (mapcar #'car rows))))
    (when (assq ghostel-tmux--current-window-id rows)
      (ghostel-tmux--leave-floating-window))))

(defun ghostel-tmux--floating-only-p ()
  "Return non-nil when the session's current window has no tiled pane."
  (and ghostel-tmux--current-window-id
       (null (plist-get (gethash ghostel-tmux--current-window-id
                                 ghostel-tmux--windows)
                        :panes))))

(defun ghostel-tmux--leave-floating-window ()
  "Find the kept window a pane when the current window has no tiled pane.
A window of floating panes only has nothing to show, so move the session
to the first window with a tiled pane: `--on-session-window-changed' then
fills the kept window with that window's own active pane, and the rows
start over when that window is gone.  Without any tiled pane, the kept
window waits as long as it holds the hidden host of a takeover; another
buffer there is the user's."
  (when (and (window-live-p ghostel-tmux--auto-host-window)
             (ghostel-tmux--floating-only-p))
    (let ((wid (cl-find-if
                (lambda (wid)
                  (plist-get (gethash wid ghostel-tmux--windows) :panes))
                ghostel-tmux--window-order)))
      (cond
       (wid (ghostel-tmux--queue-command
             (ghostel-tmux--select-window-command (current-buffer) wid)
             (lambda (output)
               (unless output
                 (ghostel-tmux--request-window-rows)))))
       ((not (eq (window-buffer ghostel-tmux--auto-host-window)
                 (current-buffer)))
        (setq ghostel-tmux--auto-host-window nil))))))

(defconst ghostel-tmux--pane-state-fields
  '(alternate_on alternate_saved_x alternate_saved_y cursor_x cursor_y
    cursor_flag wrap_flag insert_flag keypad_cursor_flag keypad_flag
    bracket_paste_flag mouse_standard_flag mouse_button_flag mouse_all_flag
    mouse_sgr_flag mouse_utf8_flag scroll_region_upper scroll_region_lower
    pane_height)
  "The tmux formats that describe a pane's terminal beyond its text.")

(defconst ghostel-tmux--pane-mode-sequences
  '((insert_flag . "\e[4h")
    (keypad_cursor_flag . "\e[?1h")
    (keypad_flag . "\e=")
    (bracket_paste_flag . "\e[?2004h")
    (mouse_standard_flag . "\e[?1000h")
    (mouse_button_flag . "\e[?1002h")
    (mouse_all_flag . "\e[?1003h")
    (mouse_sgr_flag . "\e[?1006h")
    (mouse_utf8_flag . "\e[?1005h"))
  "The sequence turning on each mode of `ghostel-tmux--pane-state-fields'.")

(defun ghostel-tmux--bootstrap-pane (controller pane-id)
  "Ask tmux, on CONTROLLER, for what it takes to backfill PANE-ID.
The pane's terminal state, its primary screen when in the alternate one,
its scrollback with its screen (a separate history capture with an empty
scrollback repeats the first screen row), and its screen alone.  One
write keeps pane output from getting between them.  `-C' octal-escapes
control bytes: %begin/%end payloads are otherwise raw, and a raw ESC
would end the control-mode DCS."
  (with-current-buffer controller
    (let ((command
           (format (concat "display-message -p -t %%%d '%s'\n"
                           "capture-pane -aq -peC -t %%%d\n"
                           "capture-pane -peJC -t %%%d -S -%d\n"
                           "capture-pane -peJC -t %%%d")
                   pane-id
                   (mapconcat (lambda (field) (format "#{%s}" field))
                              ghostel-tmux--pane-state-fields ",")
                   pane-id pane-id ghostel-tmux-history-lines pane-id))
          (replies nil))
      (letrec ((on-block
                (lambda (output)
                  (push output replies)
                  (if (< (length replies) 4)
                      (setq ghostel-tmux--command-in-flight
                            (cons command on-block))
                    (apply #'ghostel-tmux--receive-pane-backfill
                           controller pane-id (reverse replies))))))
        (ghostel-tmux--queue-command command on-block)))))

(defun ghostel-tmux--parse-pane-state (output)
  "Parse OUTPUT of `ghostel-tmux--pane-state-fields' into an alist, or nil."
  (let ((values (and output (split-string (string-trim output) ","))))
    (when (= (length values) (length ghostel-tmux--pane-state-fields))
      (cl-mapcar #'cons ghostel-tmux--pane-state-fields values))))

(defun ghostel-tmux--pane-state-number (state field)
  "Return the number of FIELD in STATE."
  (string-to-number (alist-get field state)))

(defun ghostel-tmux--cursor-sequence (x up)
  "Return the sequence moving the cursor UP rows and to column X."
  (format "%s\e[%dG" (if (> up 0) (format "\e[%dA" up) "") (1+ x)))

(defun ghostel-tmux--pane-state-sequences (state term-rows)
  "Return the sequences giving a terminal of TERM-ROWS rows the modes of STATE.
tmux resets a scroll region when the height changes, which it is about
to do for a terminal of another height.  Setting a region moves the
cursor, so it is saved around that."
  (let ((upper (ghostel-tmux--pane-state-number state 'scroll_region_upper))
        (lower (ghostel-tmux--pane-state-number state 'scroll_region_lower))
        (height (ghostel-tmux--pane-state-number state 'pane_height)))
    (concat
     (mapconcat (lambda (mode)
                  (if (equal (alist-get (car mode) state) "1") (cdr mode) ""))
                ghostel-tmux--pane-mode-sequences "")
     (if (equal (alist-get 'wrap_flag state) "0") "\e[?7l" "")
     (if (equal (alist-get 'cursor_flag state) "0") "\e[?25l" "")
     (if (and (= height term-rows)
              (or (> upper 0) (< lower (1- height))))
         (format "\e7\e[%d;%dr\e8" (1+ upper) (1+ lower))
       ""))))

(defun ghostel-tmux--backfill-rows (rows)
  "Return the captured ROWS, separated by LF, ready for the VT engine.
The engine does not synthesize CR, so they are fed as CRLF."
  (ghostel-tmux--decode-output (string-replace "\n" "\r\n" rows)))

(defun ghostel-tmux--backfill-screen (lines height x y term-rows)
  "Return the bytes showing LINES in a terminal of TERM-ROWS rows.
LINES end with a screen of HEIGHT rows whose cursor is at column X, row
Y.  The cursor moves from the last row fed: a terminal of another height
shows the rows at another offset from its top.  A lower terminal loses
blank rows below the cursor first, as tmux does when it shrinks the
pane.  A capture ends without resetting its colors."
  (let* ((below (- height y 1))
         (blank (or (cl-position-if-not #'string-blank-p (reverse lines))
                    (length lines)))
         (lost (max 0 (min below blank (- height term-rows)))))
    (concat (ghostel-tmux--backfill-rows
             (string-join (butlast lines lost) "\n"))
            "\e[m"
            (ghostel-tmux--cursor-sequence x (- below lost)))))

(defun ghostel-tmux--backfill-data (state primary rows screen term-rows)
  "Return the bytes recreating a pane in a terminal of TERM-ROWS rows.
STATE is the result of `ghostel-tmux--parse-pane-state', PRIMARY the
primary screen of a pane in the alternate one, at the height it had
there, ROWS the pane's scrollback and screen, and SCREEN the screen
alone.  Without STATE, trailing blank rows are dropped so that the
cursor lands after the last content line."
  (if (null state)
      (ghostel-tmux--backfill-rows (string-trim-right rows "\n+"))
    (let ((height (ghostel-tmux--pane-state-number state 'pane_height))
          (x (ghostel-tmux--pane-state-number state 'cursor_x))
          (y (ghostel-tmux--pane-state-number state 'cursor_y)))
      (concat
       (if (equal (alist-get 'alternate_on state) "1")
           (let* ((lines (split-string (or screen "") "\n"))
                  (history (butlast (split-string rows "\n") (length lines)))
                  (saved (split-string (or primary "") "\n")))
             (concat
              (when history
                (concat (ghostel-tmux--backfill-rows
                         (string-join history "\n"))
                        "\e[m\r\n"))
              (ghostel-tmux--backfill-screen
               saved (length saved)
               (ghostel-tmux--pane-state-number state 'alternate_saved_x)
               (ghostel-tmux--pane-state-number state 'alternate_saved_y)
               term-rows)
              "\e[?1049h\e[2J\e[H"
              (ghostel-tmux--backfill-screen lines height x y term-rows)))
         (ghostel-tmux--backfill-screen (split-string rows "\n")
                                        height x y term-rows))
       (ghostel-tmux--pane-state-sequences state term-rows)))))

(defun ghostel-tmux--receive-pane-backfill (controller pane-id state primary
                                                       rows screen)
  "Recreate PANE-ID's terminal under CONTROLLER from tmux's replies.
STATE, PRIMARY, ROWS and SCREEN are the replies `--bootstrap-pane' asked
for, each nil for a failed command."
  (let ((buf (with-current-buffer controller
               (gethash pane-id ghostel-tmux--panes))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; A pane killed while its backfill was queued gets a second one.
        (when (and rows (not ghostel-tmux--initialized))
          (let ((data (ghostel-tmux--backfill-data
                       (ghostel-tmux--parse-pane-state state) primary rows
                       screen ghostel--term-rows))
                (ghostel-tmux--in-write-vt t))
            ;; save-current-buffer: native callbacks may select another
            ;; buffer mid-parse; the state updates below must land here.
            (save-current-buffer
              (ghostel--write-vt ghostel--term data))))
        (setq ghostel-tmux--initialized t
              ghostel-tmux--bootstrap-pending nil)
        (ghostel--invalidate)))))

(defun ghostel-tmux--wipe-panes ()
  "Drop all pane buffers and reset window state."
  (let ((controller (current-buffer)))
    ;; These panes are not lost to a successor in this session.
    (setq ghostel-tmux--active-pane-id nil)
    (maphash (lambda (pid _buf)
               (ghostel-tmux--remove-pane controller pid))
             ghostel-tmux--panes)
    (clrhash ghostel-tmux--panes)
    (clrhash ghostel-tmux--windows)
    (setq ghostel-tmux--window-order nil
          ghostel-tmux--current-window-id nil
          ghostel-tmux--pending-follow-wid nil)))


;; Outbound: route encoded keystrokes to tmux send-keys -H

(defun ghostel-tmux--bytes-to-hex (data)
  "Return DATA encoded as space-separated 2-digit hex tokens.
DATA may be multibyte (e.g. a UTF-8 paste from the kill ring); it is
encoded to its UTF-8 byte representation before hex-encoding."
  (let* ((unibytes (if (multibyte-string-p data)
                       (encode-coding-string data 'utf-8)
                     data)))
    (mapconcat (lambda (b) (format "%02x" b)) unibytes " ")))

(defconst ghostel-tmux--send-keys-chunk-bytes 512
  "Maximum number of source bytes per `send-keys -H' command.
At 3 chars/byte plus the header, that is about 1.6 KB per line, well
under the typical 4 KB limit of the PTY's line discipline.")

(defun ghostel-tmux--send-bytes-from-pane (controller pane-id data)
  "Send DATA bytes to tmux PANE-ID via CONTROLLER as `send-keys -H'.
Long DATA is split into several commands, as a line longer than the
controller PTY's input buffer is truncated.  Bytes generated while
feeding `%output' are dropped, see `ghostel-tmux--in-write-vt'."
  (when (and data (> (length data) 0)
             (buffer-live-p controller)
             (not ghostel-tmux--in-write-vt))
    (let ((unibytes (if (multibyte-string-p data)
                        (encode-coding-string data 'utf-8)
                      data)))
      (with-current-buffer controller
        (when (ghostel-tmux--controller-live-p)
          (let ((i 0)
                (n (length unibytes))
                (chunk ghostel-tmux--send-keys-chunk-bytes))
            (while (< i n)
              (let* ((end (min n (+ i chunk)))
                     (hex (ghostel-tmux--bytes-to-hex
                           (substring unibytes i end))))
                (ghostel-tmux--queue-command
                 (format "send-keys -t %%%d -H %s" pane-id hex))
                (setq i end)))))))))


;; Process filter / sentinel

(defun ghostel-tmux--filter (proc string)
  "Process filter for the tmux -CC subprocess PROC; STRING is fresh output."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (ghostel-tmux--feed-bytes string)))))

(defun ghostel-tmux--sentinel (proc event)
  "Sentinel for the tmux -CC subprocess.  PROC ended with EVENT.
When the controller dies — whether cleanly or because tmux got killed
externally — seal each pane buffer so users' keystrokes don't silently
no-op against a dead process.  Panes are sealed even when the
controller buffer itself was killed before the process died.  The
controller buffer has no further use and goes away."
  (unless (process-live-p proc)
    (let* ((buf (process-buffer proc))
           (suffix "")
           ;; `ghostel-tmux-detach' deletes the process.
           (detached (and (buffer-live-p buf)
                          (string-prefix-p "killed" event)
                          (buffer-local-value 'ghostel-tmux--closing buf))))
      (ghostel-tmux--seal-panes buf)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          ;; What a tmux that never reached control mode printed.
          (when (eq ghostel-tmux--parser-state 'preamble)
            (setq ghostel-tmux--exit-reason
                  (ghostel-tmux--decode-text
                   (string-trim ghostel-tmux--line-buffer))))
          (setq suffix (ghostel-tmux--exit-suffix))
          (ghostel-tmux--refresh-tab-line))
        (kill-buffer buf))
      (message "Ghostel-tmux: %s%s"
               (if detached "Detached" (string-trim event)) suffix))))


;; Modes

(defconst ghostel-tmux--typed-reply-end "ghostel-tmux-reply-end"
  "What the marker command of `ghostel-tmux--queue-until-marker' prints.")

(defun ghostel-tmux--report-failure (command text)
  "Tell the user that tmux refused COMMAND, for the reason TEXT."
  (message "Ghostel-tmux: Command failed: %s: %s" command text))

(defun ghostel-tmux--show-reply (command outputs errors)
  "Show tmux's answer to COMMAND: the block texts OUTPUTS and ERRORS.
A plain `display-message' answers with a `%message' line."
  (let ((text (mapconcat
               (lambda (output)
                 (string-remove-prefix "%message "
                                       (ghostel-tmux--decode-text output)))
               (cl-remove-if #'string-blank-p outputs)
               "\n"))
        (inhibit-read-only t))
    (unless (string-empty-p text)
      (display-message-or-buffer text "*ghostel-tmux-output*")))
  (when errors
    (ghostel-tmux--report-failure command (string-join errors "\n"))))

(defun ghostel-tmux--queue-until-marker (command on-block &optional on-done)
  "Queue COMMAND, then a marker command, and stay in flight until its block.
COMMAND may answer with any number of blocks.  ON-BLOCK is called with
the payload of each block before the marker's, nil for an error, and
ON-DONE then without arguments."
  (letrec ((cont (lambda (output)
                   (cond
                    ((equal output ghostel-tmux--typed-reply-end)
                     (when on-done (funcall on-done)))
                    (t
                     (funcall on-block output)
                     (setq ghostel-tmux--command-in-flight
                           (cons command cont)))))))
    (ghostel-tmux--queue-command
     (format "%s\ndisplay-message -p %s" command ghostel-tmux--typed-reply-end)
     cont)))

(defun ghostel-tmux--queue-typed-command (command)
  "Queue COMMAND, written by the user, and show what tmux answers."
  (let (outputs errors)
    (ghostel-tmux--queue-until-marker
     command
     (lambda (output)
       (if output
           (push output outputs)
         (push ghostel-tmux--reply-error errors)))
     (lambda ()
       (ghostel-tmux--show-reply command (reverse outputs) (reverse errors))))))

(defun ghostel-tmux--cmd (command &optional typed)
  "Run tmux command-mode COMMAND through the active controller.
A `%error' reply is reported to the user.  TYPED marks a COMMAND the
user wrote, see `ghostel-tmux--queue-typed-command'."
  (let ((controller (ghostel-tmux--active-controller)))
    (unless (buffer-live-p controller)
      (user-error "Ghostel-tmux: No active controller"))
    (with-current-buffer controller
      (unless (ghostel-tmux--controller-live-p)
        (user-error "Ghostel-tmux: Controller is not connected"))
      (if typed
          (ghostel-tmux--queue-typed-command command)
        (ghostel-tmux--queue-command
         command
         (lambda (output)
           (unless output
             (ghostel-tmux--report-failure
              command ghostel-tmux--reply-error))))))))

(defun ghostel-tmux--prefix-target ()
  "Return a `-t' target spec for the current pane buffer's tmux pane."
  (when ghostel-tmux--pane-id
    (format "%%%d" ghostel-tmux--pane-id)))

(defun ghostel-tmux--session-target ()
  "Return `$SID' for the controller of the current pane buffer, or nil.
Used for `-t' on session/window-level commands (`next-window',
`new-window', …) where a pane target like `%3' is not accepted."
  (let ((controller ghostel-tmux--controller-buffer))
    (and (buffer-live-p controller)
         (with-current-buffer controller
           (and ghostel-tmux--session-id
                (format "$%d" ghostel-tmux--session-id))))))

(defun ghostel-tmux--select-window-command (controller window-id)
  "Return the command selecting window WINDOW-ID in CONTROLLER's session.
A bare `@N' resolves to the most recently active session holding the
window, which can be a session grouped with this one."
  (let ((sid (and (buffer-live-p controller)
                  (buffer-local-value 'ghostel-tmux--session-id controller))))
    (format "select-window -t %s@%d"
            (if sid (format "$%d:" sid) "")
            window-id)))

(defun ghostel-tmux--window-target ()
  "Return `@WID' for the current pane's window, or nil."
  (when ghostel-tmux--window-id
    (format "@%d" ghostel-tmux--window-id)))

(defmacro ghostel-tmux--define-prefix-cmd (slug key tmux-cmd-fn doc)
  "Bind KEY in `ghostel-tmux-prefix-map' to a tmux command builder.
SLUG is appended to the generated function name.  TMUX-CMD-FN is
called with the pane target (e.g. \"%3\") and must return the tmux
command string to send.  DOC is the docstring."
  (let ((sym (intern (format "ghostel-tmux--prefix-%s" slug))))
    `(progn
       (defun ,sym ()
         ,doc
         (interactive)
         (ghostel-tmux--cmd
          (funcall ,tmux-cmd-fn (ghostel-tmux--prefix-target))))
       (define-key ghostel-tmux-prefix-map (kbd ,key) #',sym))))

(defvar ghostel-tmux-prefix-map (make-sparse-keymap)
  "Keymap entered after the tmux prefix key.
The prefix key itself is `ghostel-tmux-prefix-key' (default `C-t').
Mirrors common tmux prefix bindings so that the muscle memory works
inside ghostel pane buffers.  The bindings are translated into tmux
control-mode commands sent over the active controller.")

(ghostel-tmux--define-prefix-cmd split-h "\""
  (lambda (target) (format "split-window -v -t %s" target))
  "Split the current pane horizontally (new pane below).")

(ghostel-tmux--define-prefix-cmd split-v "%"
  (lambda (target) (format "split-window -h -t %s" target))
  "Split the current pane vertically (new pane right).")

(ghostel-tmux--define-prefix-cmd new-window "c"
  (lambda (_target)
    (let ((sess (ghostel-tmux--session-target)))
      (if sess
          (format "new-window -t %s" sess)
        "new-window")))
  "Create a new tmux window.")

(ghostel-tmux--define-prefix-cmd kill-pane "x"
  (lambda (target) (format "kill-pane -t %s" target))
  "Kill the current pane.")

(ghostel-tmux--define-prefix-cmd kill-window "&"
  (lambda (_target)
    (let ((win (ghostel-tmux--window-target)))
      (if win
          (format "kill-window -t %s" win)
        "kill-window")))
  "Kill the current window.")

(defun ghostel-tmux--cycle-window (command)
  "Run COMMAND, `next-window' or `previous-window', on this session.
tmux knows the window order, which reaches us late or not at all.
The visible switch happens reactively in `--on-session-window-changed'."
  (let ((controller ghostel-tmux--controller-buffer)
        (target (ghostel-tmux--session-target)))
    (unless target
      (user-error "Ghostel-tmux: No controller/window context"))
    (when (cdr (buffer-local-value 'ghostel-tmux--window-order controller))
      (ghostel-tmux--cmd (format "%s -t %s" command target)))))

(defun ghostel-tmux--prefix-next-window ()
  "Switch to the next tmux window and display its active pane."
  (interactive)
  (ghostel-tmux--cycle-window "next-window"))
(define-key ghostel-tmux-prefix-map (kbd "n") #'ghostel-tmux--prefix-next-window)

(defun ghostel-tmux--prefix-prev-window ()
  "Switch to the previous tmux window and display its active pane."
  (interactive)
  (ghostel-tmux--cycle-window "previous-window"))
(define-key ghostel-tmux-prefix-map (kbd "p") #'ghostel-tmux--prefix-prev-window)

(ghostel-tmux--define-prefix-cmd next-pane "o"
  (lambda (_target) "select-pane -t :.+")
  "Cycle to the next pane in the current window.")

(ghostel-tmux--define-prefix-cmd rename-window ","
  (lambda (_target)
    (let ((name (read-string "rename window to: "))
          (win (ghostel-tmux--window-target)))
      (if win
          (format "rename-window -t %s %s" win
                  (shell-quote-argument name))
        (format "rename-window %s" (shell-quote-argument name)))))
  "Rename the current tmux window.")

(ghostel-tmux--define-prefix-cmd rename-session "$"
  (lambda (_target)
    (let ((name (read-string "rename session to: ")))
      (format "rename-session %s" (shell-quote-argument name))))
  "Rename the current tmux session.")

(defun ghostel-tmux--prefix-cmd ()
  "Prompt for an arbitrary tmux command, send it and show its output."
  (interactive)
  ;; An empty line makes tmux leave control mode.
  (when-let* ((lines (split-string (read-string "tmux command: ")
                                   "\n" t "[ \t]+")))
    (ghostel-tmux--cmd (string-join lines "\n") t)))
(define-key ghostel-tmux-prefix-map (kbd ":") #'ghostel-tmux--prefix-cmd)

;; Number keys: select tmux window N in the current session.
(dotimes (n 10)
  (let ((key (number-to-string n))
        (idx n))
    (define-key ghostel-tmux-prefix-map (kbd key)
      (let ((fn (intern (format "ghostel-tmux--prefix-window-%d" idx))))
        (defalias fn
          (lambda ()
            (interactive)
            (ghostel-tmux--cmd
             (format "select-window -t %s:%d"
                     (or (ghostel-tmux--session-target) "") idx)))
          (format "Switch to window %d in the current tmux session." idx))
        fn))))

;; Detach as in tmux, panes in tab order across tmux windows, and the
;; pane picker.
(define-key ghostel-tmux-prefix-map (kbd "d") #'ghostel-tmux-detach)
(define-key ghostel-tmux-prefix-map (kbd "C-n") #'ghostel-tmux-next-pane)
(define-key ghostel-tmux-prefix-map (kbd "C-p") #'ghostel-tmux-prev-pane)
(define-key ghostel-tmux-prefix-map (kbd "s") #'ghostel-tmux-switch-pane)

(defvar ghostel-tmux-pane-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-n") #'ghostel-tmux-next-pane)
    (define-key map (kbd "C-p") #'ghostel-tmux-prev-pane)
    map)
  "Keymap to repeat the pane cycling commands in `repeat-mode'.")
(dolist (cmd '(ghostel-tmux-next-pane ghostel-tmux-prev-pane))
  (put cmd 'repeat-map 'ghostel-tmux-pane-repeat-map))

;; `<prefix> ?': describe the prefix map.
(define-key ghostel-tmux-prefix-map (kbd "?")
            (lambda ()
              (interactive)
              (describe-keymap 'ghostel-tmux-prefix-map)))

(defun ghostel-tmux--prefix-self-insert ()
  "Send the tmux prefix key itself to the underlying program.
Bound as `<prefix> <prefix>', matching tmux's convention.  Only
meaningful when `ghostel-tmux-prefix-key' resolves to a single control
byte (`C-<letter>')."
  (interactive)
  (let* ((seq (kbd ghostel-tmux-prefix-key))
         (byte (and (= (length seq) 1)
                    (let ((e (aref seq 0)))
                      (and (integerp e) (<= 0 e 31) e)))))
    (if byte
        (ghostel--send-string (string byte))
      (message "Ghostel-tmux: %S is not a control byte; cannot send raw"
               ghostel-tmux-prefix-key))))

(defvar ghostel-tmux-pane-mode-map (make-sparse-keymap)
  "Keymap for `ghostel-tmux-pane-mode': the tmux prefix.")

(defun ghostel-tmux--install-prefix-key (key)
  "Bind KEY in `ghostel-tmux-pane-mode-map' to the tmux prefix commands.
The map is a child of `ghostel-tmux-prefix-map' in which KEY twice sends
the prefix byte to the underlying program."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ghostel-tmux-prefix-map)
    (define-key map (kbd key) #'ghostel-tmux--prefix-self-insert)
    (define-key ghostel-tmux-pane-mode-map (kbd key) map)))

(ghostel-tmux--install-prefix-key ghostel-tmux-prefix-key)

(define-minor-mode ghostel-tmux-pane-mode
  "Minor mode making this ghostel buffer act as a tmux pane.
Activated when the controller materializes a pane buffer.  Composes
with the active ghostel input mode (semi-char, line, or
emacs-mode) — the tmux prefix is added without replacing the
terminal's key bindings.  In char mode the prefix is intentionally
inert so a TUI program inside the pane receives the keystroke raw."
  :lighter " Tmux"
  :keymap ghostel-tmux-pane-mode-map
  (cond
   (ghostel-tmux-pane-mode
    (setq-local tab-line-tabs-function #'ghostel-tmux--tab-line-tabs)
    (tab-line-mode 1)
    (add-hook 'kill-buffer-hook #'ghostel-tmux--pane-killed nil t)
    ;; tab-line click and any other plain `switch-to-buffer' path
    ;; won't go through `--display-pane-in-window'; this hook makes
    ;; sure the pane terminal is sized whenever its buffer becomes a
    ;; window's content.
    (add-hook 'window-buffer-change-functions
              #'ghostel-tmux--on-window-buffer-change nil t))
   (t
    (ghostel-tmux--drop-tab-line)
    (remove-hook 'kill-buffer-hook #'ghostel-tmux--pane-killed t)
    (remove-hook 'window-buffer-change-functions
                 #'ghostel-tmux--on-window-buffer-change t))))

(defun ghostel-tmux--drop-tab-line ()
  "Turn off the pane tabs of this buffer, which has no siblings to list."
  (tab-line-mode -1)
  (kill-local-variable 'tab-line-tabs-function))

(defun ghostel-tmux--refresh-tab-line ()
  "Refresh `tab-line-mode' display in every pane buffer of this controller."
  (when ghostel-tmux--panes
    (maphash (lambda (_pid buf)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (force-mode-line-update t))))
             ghostel-tmux--panes)))

(define-derived-mode ghostel-tmux-control-mode special-mode "Ghostel-tmux-ctl"
  "Hidden buffer hosting the tmux -CC subprocess."
  (buffer-disable-undo)
  (setq buffer-read-only t
        ghostel-tmux--panes (make-hash-table)
        ghostel-tmux--windows (make-hash-table))
  ;; A killed controller buffer must not leave a zombie tmux -CC; the
  ;; sentinel then seals the panes.
  (add-hook 'kill-buffer-hook #'ghostel-tmux--controller-killed nil t))

(defun ghostel-tmux--controller-killed ()
  "Tear down the tmux -CC subprocess when its controller buffer dies."
  (when (and ghostel-tmux--controller-process
             (process-live-p ghostel-tmux--controller-process))
    (delete-process ghostel-tmux--controller-process)))

(defun ghostel-tmux--pane-killed ()
  "When a pane buffer dies, evict it from the controller.
The pane lives on in tmux, so a buffer the user killed comes back."
  (let ((controller ghostel-tmux--controller-buffer)
        (pid ghostel-tmux--pane-id)
        (pane (current-buffer)))
    (when (and pid (buffer-live-p controller))
      (with-current-buffer controller
        (when (and ghostel-tmux--panes
                   (eq (gethash pid ghostel-tmux--panes) pane))
          (remhash pid ghostel-tmux--panes)
          (when (ghostel-tmux--controller-live-p)
            (ghostel-tmux--request-window-rows)))))))


;; Sizing

(defun ghostel-tmux--commit-hidden-resize ()
  "Commit a resize of this pane, which no window shows.
The terminal commits a resize on its next redraw and clears the prompt
row then; that redraw would clear the prompt the shell repaints after
tmux has resized the pane.  This render has no window of the pane and
loses glyph sizing, so showing the pane repaints it in full.  A pane in
copy or line mode cannot be rendered directly and keeps the resize
pending."
  (unless (memq ghostel--input-mode '(copy line))
    (ghostel--commit-frame (current-buffer))
    (setq ghostel--repair-pending t
          ghostel--pending-redraw t)))

(defun ghostel-tmux--resize-pane (rows cols)
  "Resize this pane's terminal to ROWS by COLS and report the size to tmux.
The pane's `ghostel-resize-function'.  An unchanged grid still passes
through, for the cell pixel size and the report: tmux's client size may
be that of another pane's window."
  (let ((same-size (and (eql rows ghostel--term-rows)
                        (eql cols ghostel--term-cols))))
    (ghostel--set-size-with-cell-dims ghostel--term rows cols)
    (setq ghostel--term-rows rows
          ghostel--term-cols cols)
    ;; The terminal commits a resize on its next redraw and clears the
    ;; prompt row then.  Commit now: after tmux has resized the pane, that
    ;; would clear the prompt the shell has just repainted.
    (if same-size
        (ghostel--invalidate)
      (ghostel--redraw-now (current-buffer) t)))
  (let ((controller ghostel-tmux--controller-buffer))
    (when (buffer-live-p controller)
      (with-current-buffer controller
        (when (ghostel-tmux--controller-live-p)
          (ghostel-tmux--queue-command
           (format "refresh-client -C %d,%d" cols rows)))))))


;; Tab line

(defun ghostel-tmux--tab-line-tabs ()
  "Return tabs (sibling pane buffers) for `tab-line-mode' in this buffer."
  (when (buffer-live-p ghostel-tmux--controller-buffer)
    (ghostel-tmux--pane-list ghostel-tmux--controller-buffer)))


;; Takeover: a regular ghostel buffer running `tmux -CC' becomes a controller

;; ghostel calls `ghostel-control-mode-function' when the DCS-1000p frame
;; (start: `\\eP1000p', end: `\\e\\\\') opens, with its body bytes once
;; per input chunk, and when it closes.

;;;###autoload
(define-minor-mode ghostel-tmux-takeover-mode
  "Turn `tmux -CC' typed in a ghostel shell into pane buffers.
The shell's buffer hides itself while tmux is attached and comes back
when tmux detaches.  Turning the mode off stops new takeovers; a session
already attached keeps running until tmux detaches."
  :global t
  :group 'ghostel-tmux
  (when ghostel-tmux-takeover-mode
    (setq ghostel-control-mode-function #'ghostel-tmux--control)))

(defun ghostel-tmux--control (event data)
  "Handle control-mode EVENT with DATA in the current terminal buffer.
The `ghostel-control-mode-function' of `ghostel-tmux-takeover-mode'.
It stays installed when the mode is off, as a running session needs its
frame's data and exit."
  (pcase event
    ('enter
     (when (and ghostel-tmux-takeover-mode (not (ghostel-tmux--dcs-enter)))
       (message (concat "Ghostel-tmux: Ignoring tmux control-mode output in %s; "
                        "it stays hidden until tmux detaches "
                        "(RET in that pane ends that client)")
                (buffer-name))))
    ('data (ghostel-tmux--dcs-data data))
    ('exit (ghostel-tmux--dcs-exit))))

(defun ghostel-tmux-unload-function ()
  "Stop ghostel calling `ghostel-tmux--control' before this file unloads."
  (when (eq ghostel-control-mode-function #'ghostel-tmux--control)
    (setq ghostel-control-mode-function nil))
  nil)

(defun ghostel-tmux--dcs-enter ()
  "Handle a tmux control-mode DCS opening in the current buffer.
Enters control mode unless we're already inside a tmux-related buffer
where DCS doesn't make sense (a pane buffer with `%output' that happens
to contain `\\eP1000p', or the dedicated controller's hidden buffer).
Returns non-nil when control mode was entered or is already active in
this buffer, nil when the takeover is declined."
  (cond
   (ghostel-tmux--dcs-active t)
   ((and (not ghostel-tmux-pane-mode)
         (not (derived-mode-p 'ghostel-tmux-control-mode)))
    (setq ghostel-tmux--dcs-active t)
    (ghostel-tmux--enter-control-mode (current-buffer))
    t)))

(defun ghostel-tmux--dcs-data (data)
  "Handle a chunk of tmux DCS body bytes (DATA).
Forwards to the control-mode parser only when this buffer is
currently inside a DCS frame we entered via
`ghostel-tmux--dcs-enter'."
  (when ghostel-tmux--dcs-active
    (ghostel-tmux--feed-bytes data)))

(defun ghostel-tmux--dcs-exit ()
  "Handle the active tmux DCS closing.
Tears down control-mode state and reverts the host buffer to a
plain shell.  A frame ending without `%exit' was cut short by an ESC in
a reply while tmux stays attached: an empty line makes it leave."
  (when ghostel-tmux--dcs-active
    (let ((closing ghostel-tmux--closing))
      (setq ghostel-tmux--dcs-active nil)
      (ghostel-tmux--exit-control-mode (current-buffer))
      (when (and (not closing) ghostel--term (process-live-p ghostel--process))
        (ghostel--write-pty ghostel--term "\n")))))

(defun ghostel-tmux--reset-session-state ()
  "Forget the parser, the command queue and the session's windows."
  (setq ghostel-tmux--line-buffer ""
        ghostel-tmux--block-lines nil
        ghostel-tmux--block-tag nil
        ghostel-tmux--command-queue nil
        ghostel-tmux--command-in-flight nil
        ghostel-tmux--window-order nil
        ghostel-tmux--current-window-id nil
        ghostel-tmux--pending-follow-wid nil
        ghostel-tmux--active-pane-id nil))

(defun ghostel-tmux--enter-control-mode (buf)
  "Take over BUF (a ghostel buffer) as a tmux controller.
Initializes tmux controller state in-place: commands ride the host
terminal's PTY (via `ghostel-tmux--controller-send'), and pane buffers
are created as the parser sees notifications.  The host is hidden, see
`ghostel-tmux--auto-original-name'."
  (with-current-buffer buf
    (let ((original-name (buffer-name)))
      (setq ghostel-tmux--auto-original-name original-name)
      (rename-buffer (format " *ghostel-tmux-host:%s*" original-name) t))
    (setq ghostel-tmux--host-identity ghostel-identity
          ghostel-identity nil)
    (add-hook 'ghostel-exit-functions #'ghostel-tmux--host-exited nil t)
    (add-hook 'kill-buffer-hook #'ghostel-tmux--host-killed nil t)
    (add-hook 'window-buffer-change-functions #'ghostel-tmux--hide-host nil t)
    ;; `ghostel-tmux--controller-process' stays nil: the control stream
    ;; rides the host PTY, which `ghostel-tmux--controller-send' reaches
    ;; through `ghostel--write-pty'.
    (ghostel-tmux--reset-session-state)
    (setq ghostel-tmux--controller-process nil
          ghostel-tmux--session-name ghostel-tmux--auto-original-name
          ghostel-tmux--parser-state 'idle
          ghostel-tmux--closing nil
          ghostel-tmux--exit-reason nil
          ghostel-tmux--session-id nil
          ghostel-tmux--panes (make-hash-table)
          ghostel-tmux--windows (make-hash-table)
          ghostel-tmux--auto-host-window (get-buffer-window buf t))
    (message "Ghostel-tmux: Entered control mode (buffer %s)"
             (buffer-name buf))))

(defun ghostel-tmux--exit-control-mode (buf)
  "Restore BUF to normal ghostel operation after tmux exited control mode.
Pane buffers are sealed as read-only snapshots, and any windows showing
them are returned to the host buffer so the user lands back at the shell."
  (with-current-buffer buf
    (let (pane-buffers)
      (when ghostel-tmux--panes
        (maphash (lambda (_pid pane-buf)
                   (when (buffer-live-p pane-buf)
                     (push pane-buf pane-buffers)
                     (ghostel-tmux--seal-pane-buffer pane-buf)))
                 ghostel-tmux--panes))
      ;; Any window currently showing one of our pane buffers should
      ;; switch back to the host so the user sees the shell prompt again.
      (walk-windows
       (lambda (window)
         (when (memq (window-buffer window) pane-buffers)
           (with-selected-window window
             (switch-to-buffer buf))))
       'no-minibuffer 'visible))
    (ghostel-tmux--reset-session-state)
    (setq ghostel-tmux--controller-process nil
          ghostel-tmux--parser-state 'preamble
          ghostel-tmux--panes nil
          ghostel-tmux--windows nil)
    (remove-hook 'ghostel-exit-functions #'ghostel-tmux--host-exited t)
    (remove-hook 'kill-buffer-hook #'ghostel-tmux--host-killed t)
    (remove-hook 'window-buffer-change-functions #'ghostel-tmux--hide-host t)
    ;; Restore the host buffer's original (visible) name, if we renamed
    ;; it during entry to hide it from the user-visible buffer list.
    (when ghostel-tmux--auto-original-name
      (let ((original ghostel-tmux--auto-original-name))
        (setq ghostel-tmux--auto-original-name nil)
        (ghostel-tmux--restore-identity)
        (ignore-errors (rename-buffer original t))))
    (message "Ghostel-tmux: Exited control mode (buffer %s)%s"
             (buffer-name buf) (ghostel-tmux--exit-suffix))))

(defun ghostel-tmux--host-exited (buf _event)
  "Seal pane state when the auto-takeover host BUF's shell dies.
Runs from `ghostel-exit-functions' (added buffer-locally on control-mode
entry): with the host process gone there is no control channel left, so
panes are sealed read-only and the host reverts to a plain buffer."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when ghostel-tmux--dcs-active
        (setq ghostel-tmux--dcs-active nil)
        (ghostel-tmux--exit-control-mode buf)))))

(defun ghostel-tmux--restore-identity ()
  "Give this host its `ghostel-identity' back, as a new instance if taken."
  (let ((identity ghostel-tmux--host-identity))
    (when (and (assq 'instance identity)
               (ghostel--find-buffer-by-identity identity))
      (let ((context (assq-delete-all 'instance (copy-alist identity))))
        (setq identity (cons (cons 'instance (ghostel--next-instance context))
                             context))))
    (setq ghostel-identity identity
          ghostel-tmux--host-identity nil)))

(defun ghostel-tmux--hide-host (window)
  "Show WINDOW's previous buffer in place of a takeover host.
What is typed in the host would reach tmux as commands.  The host stays
in `ghostel-tmux--auto-host-window', where a pane is to replace it, and
in a dedicated window."
  (let ((host (and (window-live-p window) (window-buffer window))))
    (when (and host
               (buffer-local-value 'ghostel-tmux--dcs-active host)
               (not (eq window (buffer-local-value
                                'ghostel-tmux--auto-host-window host)))
               (memq (window-dedicated-p window) '(nil side)))
      (bury-buffer-internal host)
      ;; Window change functions are done with WINDOW for this round, so
      ;; the buffer shown now would miss its own.
      (switch-to-prev-buffer window t)
      (let ((buf (window-buffer window)))
        (dolist (fn (and (not (eq buf host))
                         (local-variable-p 'window-buffer-change-functions buf)
                         (buffer-local-value 'window-buffer-change-functions
                                             buf)))
          (unless (eq fn t)
            (with-current-buffer buf
              (funcall fn window))))))))

(defun ghostel-tmux--host-killed ()
  "Seal the panes of this auto-takeover host, whose buffer is being killed.
`ghostel-exit-functions' does not run for a buffer that is gone."
  (ghostel-tmux--seal-panes (current-buffer)))


;; Public commands

(defun ghostel-tmux--process-file (&rest args)
  "Run tmux with ARGS, insert its stdout at point and return its exit status.
tmux's text is UTF-8 whatever the locale."
  (let ((coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix))
    (apply #'process-file ghostel-tmux-program nil (list t nil) nil "-u" args)))

(defun ghostel-tmux--read-session ()
  "Prompt for a tmux session name and return it.
A non-zero exit (no server running) counts as no sessions."
  (let* ((status nil)
         (output (with-output-to-string
                   (with-current-buffer standard-output
                     (setq status
                           (ghostel-tmux--process-file
                            "list-sessions" "-F" "#{session_name}")))))
         (sessions (and (eql status 0) (split-string output "\n" t)))
         (default (car sessions)))
    (cond
     ((null sessions)
      (user-error "Ghostel-tmux: No tmux sessions found"))
     ((= (length sessions) 1) (car sessions))
     (t (completing-read "tmux session: " sessions nil t nil nil default)))))

(defun ghostel-tmux--load-module ()
  "Load ghostel's native module, or signal `user-error' without it.
`ghostel--load-module' returns normally when the user skips the install."
  (ghostel--load-module t)
  (unless (fboundp 'ghostel--new)
    (user-error "Ghostel-tmux: The native ghostel module is not available")))

(defun ghostel-tmux--check-spawn ()
  "Signal `user-error' when no controller can start in `default-directory'."
  (ghostel-tmux--load-module)
  ;; A remote shell that cannot `cd' there stays alive at its prompt.
  (unless (file-directory-p default-directory)
    (user-error "Ghostel-tmux: No directory %s" default-directory)))

(defun ghostel-tmux--spawn-controller (session tmux-args)
  "Spawn a tmux -CC subprocess as a controller buffer.
SESSION is the user-visible session label.  TMUX-ARGS is the list
of arguments after \"tmux -CC\".  `-u' keeps non-ASCII text intact in
any locale.  A remote `default-directory' runs tmux on that host.
Returns the controller buffer."
  (let ((control-buf (generate-new-buffer
                      (format " *ghostel-tmux-control:%s*" session))))
    (condition-case err
        (with-current-buffer control-buf
          (ghostel-tmux-control-mode)
          (let* ((proc (make-process
                        :name (format "ghostel-tmux-%s" session)
                        :buffer control-buf
                        :command `(,ghostel-tmux-program "-u" "-CC" ,@tmux-args)
                        :coding 'binary
                        :connection-type 'pty
                        :file-handler t
                        :noquery t
                        :filter #'ghostel-tmux--filter
                        :sentinel #'ghostel-tmux--sentinel)))
            ;; TRAMP sets the coding of its connection after `:coding'.
            (set-process-coding-system proc 'binary 'binary)
            (setq ghostel-tmux--controller-process proc
                  ghostel-tmux--session-name session)))
      ((error quit)
       (kill-buffer control-buf)
       (signal (car err) (cdr err))))
    control-buf))

(defun ghostel-tmux--apply-pane-size (pane-buf window)
  "Fit PANE-BUF's terminal to WINDOW showing it and tell tmux about the size.
Splitting and tab-line switches both hit this path, so a pane that
was created at default geometry gets corrected the moment it becomes
visible in an Emacs window.  The size is reported even when the grid
already fits, because tmux's client size may still be that of another
pane's window.  tmux is asked to switch to the pane, and when its
window has more than one pane, to zoom it to the full client size:
otherwise a split leaves half a pane in a full-height Emacs window."
  (when (and (window-live-p window) (buffer-live-p pane-buf))
    (with-current-buffer pane-buf
      (let ((pane-id ghostel-tmux--pane-id)
            (window-id ghostel-tmux--window-id)
            (controller ghostel-tmux--controller-buffer))
        (ghostel--adjust-size window t)
        (when (and pane-id window-id (buffer-live-p controller))
          (with-current-buffer controller
            (when (ghostel-tmux--controller-live-p)
              (ghostel-tmux--select-pane window-id pane-id)
              (ghostel-tmux--ensure-pane-zoomed window-id pane-id))))))))

(defun ghostel-tmux--send-selection (controller window-id pane-id)
  "Ask tmux, through CONTROLLER, for PANE-ID in window WINDOW-ID.
The pane goes first, so that `%session-window-changed' finds it recorded
as the window's active pane."
  (ghostel-tmux--cmd (format "select-pane -t %%%d" pane-id))
  (when window-id
    (ghostel-tmux--cmd
     (ghostel-tmux--select-window-command controller window-id))))

(defun ghostel-tmux--select-pane (window-id pane-id)
  "Ask tmux to switch to PANE-ID in window WINDOW-ID, which Emacs now shows.
Does nothing for `ghostel-tmux--active-pane-id', the pane tmux already
has or the one standing in for a floating pane: answering a report with
a selection would chase a newer selection forever.  The selection
replaces a deferred follow."
  (unless (eql pane-id ghostel-tmux--active-pane-id)
    (ghostel-tmux--send-selection (current-buffer) window-id pane-id)
    (setq ghostel-tmux--current-window-id window-id
          ghostel-tmux--active-pane-id pane-id
          ghostel-tmux--pending-follow-wid nil)))

(defun ghostel-tmux--set-zooming (window-id token)
  "Record TOKEN as the zoom in progress in window WINDOW-ID."
  (when-let* ((win (gethash window-id ghostel-tmux--windows)))
    (puthash window-id (plist-put win :zooming token)
             ghostel-tmux--windows)))

(defun ghostel-tmux--zooming-p (window-id token)
  "Non-nil while TOKEN is the zoom in progress in window WINDOW-ID."
  (eq token (plist-get (gethash window-id ghostel-tmux--windows) :zooming)))

(defun ghostel-tmux--finish-zooming (window-id token)
  "End the zoom TOKEN in window WINDOW-ID unless a newer one took over."
  (when (ghostel-tmux--zooming-p window-id token)
    (ghostel-tmux--set-zooming window-id nil)))

(defun ghostel-tmux--cancel-zooming (window-id pane-id)
  "Drop a zoom in progress in window WINDOW-ID unless it is of PANE-ID.
Called when a selection of PANE-ID is queued: `resize-pane -Z' makes
its pane active, which would undo that selection."
  (let ((token (plist-get (gethash window-id ghostel-tmux--windows) :zooming)))
    (when (and token (not (eql (car token) pane-id)))
      (ghostel-tmux--set-zooming window-id nil))))

(defun ghostel-tmux--ensure-pane-zoomed (window-id pane-id)
  "Ask tmux to zoom PANE-ID in window WINDOW-ID unless the window is zoomed.
Single-pane windows are skipped: zoom is a no-op there.  `resize-pane -Z'
toggles, so the window's zoom flag is read first.  The window's :zooming
entry is a fresh (PANE-ID) token per zoom in progress: a repeat for that
pane is dropped, as a second toggle would undo the first, and the steps
of a replaced token do nothing."
  (let ((win (gethash window-id ghostel-tmux--windows))
        (token (list pane-id)))
    (when (and win
               (> (length (plist-get win :panes)) 1)
               (not (eql pane-id (car (plist-get win :zooming)))))
      (ghostel-tmux--set-zooming window-id token)
      (ghostel-tmux--queue-command
       (format "display-message -p -t %%%d '#{window_zoomed_flag}'" pane-id)
       (lambda (output)
         (if (and output
                  (equal (string-trim output) "0")
                  (ghostel-tmux--zooming-p window-id token))
             (ghostel-tmux--queue-command
              (format "resize-pane -Z -t %%%d" pane-id)
              (lambda (_output)
                (ghostel-tmux--finish-zooming window-id token)))
           (ghostel-tmux--finish-zooming window-id token)))))))

(defun ghostel-tmux--display-pane-in-window (pane-buf window)
  "Display PANE-BUF in WINDOW and resize its terminal to fit."
  (when (and (window-live-p window) (buffer-live-p pane-buf))
    (with-selected-window window
      (switch-to-buffer pane-buf t))
    (ghostel-tmux--apply-pane-size pane-buf window)))

(defun ghostel-tmux--on-window-buffer-change (window)
  "Resize the pane terminal when this buffer becomes WINDOW's content.
Hook for `window-buffer-change-functions': covers tab-line clicks and
any other path that swaps the buffer into a window without going
through `ghostel-tmux--display-pane-in-window'.  From Emacs 31 the hook
runs in its buffer, also for the buffer WINDOW lost; before, in whatever
buffer is current."
  (when (and (window-live-p window)
             (or (< emacs-major-version 31)
                 (eq (window-buffer window) (current-buffer))))
    (let ((pane (window-buffer window)))
      (when (buffer-live-p (buffer-local-value
                            'ghostel-tmux--controller-buffer pane))
        (ghostel-tmux--apply-pane-size pane window)))))

(defun ghostel-tmux--attach-pane ()
  "Return the buffer of tmux's active pane, or the one standing in for it.
A floating active pane has no buffer: the first pane of the current window
takes its place, recorded as active so that showing it selects nothing."
  (or (gethash ghostel-tmux--active-pane-id ghostel-tmux--panes)
      (let* ((window (gethash ghostel-tmux--current-window-id
                              ghostel-tmux--windows))
             (pid (car (plist-get window :panes)))
             (buf (and pid (gethash pid ghostel-tmux--panes))))
        (when (and buf ghostel-tmux--active-pane-id)
          (setq ghostel-tmux--active-pane-id pid)
          buf))))

(defun ghostel-tmux--wait-for-pane (control-buf timeout)
  "Block up to TIMEOUT seconds for CONTROL-BUF to materialize a pane.
Switches the selected window to tmux's active pane when one appears,
or to the first pane while the active one is unknown.  A current window
of floating panes only keeps the selected window for a window it moves
to.  A tmux that dies ends the wait; the sentinel reports it.  A pane
later than TIMEOUT still gets the window."
  (let ((target-window (selected-window))
        (deadline (+ (float-time) timeout))
        (proc (buffer-local-value 'ghostel-tmux--controller-process
                                  control-buf))
        panes)
    (while (and (buffer-live-p control-buf)
                (not (setq panes (ghostel-tmux--pane-list control-buf)))
                (process-live-p proc)
                (< (float-time) deadline))
      (accept-process-output proc 0.1))
    (cond
     (panes
      (with-current-buffer control-buf
        (let ((pane (ghostel-tmux--attach-pane)))
          (if (and (null pane)
                   ghostel-tmux--active-pane-id
                   (ghostel-tmux--floating-only-p))
              (progn
                (setq ghostel-tmux--auto-host-window target-window)
                (ghostel-tmux--leave-floating-window))
            (ghostel-tmux--display-pane-in-window (or pane (car panes))
                                                  target-window)))))
     ((and (process-live-p proc) (buffer-live-p control-buf))
      (with-current-buffer control-buf
        (setq ghostel-tmux--auto-host-window target-window))
      (message "Ghostel-tmux: Still waiting for tmux")))))

;; A bare name is parsed as a target: `proj.0' would be pane 0 of `proj'.
(defun ghostel-tmux--exact-session-target (session)
  "Return the tmux target of the session named exactly SESSION."
  (format "=%s:" session))

(defun ghostel-tmux--session-exists-p (session)
  "Return non-nil when tmux has a session named exactly SESSION."
  (eql 0 (ignore-errors
           (with-temp-buffer
             (ghostel-tmux--process-file
              "has-session"
              "-t" (ghostel-tmux--exact-session-target session))))))

(defun ghostel-tmux--attach-args (session)
  "Return the tmux arguments that attach the session named exactly SESSION."
  (list "attach-session" "-t" (ghostel-tmux--exact-session-target session)))

(defun ghostel-tmux--literal-format (string)
  "Return the tmux format that expands to STRING.
tmux leaves a run of `#' before `[' as it is, as a style."
  (replace-regexp-in-string
   "#+\\[?"
   (lambda (match) (if (string-suffix-p "[" match) match (concat match match)))
   string t t))

;;;###autoload
(defun ghostel-tmux-attach (session)
  "Attach to the tmux SESSION via control mode and materialize its panes.
SESSION is an exact name.  tmux cannot address a name that starts with
`$' or contains `:' this way."
  (interactive (list (ghostel-tmux--read-session)))
  (ghostel-tmux--check-spawn)
  (ghostel-tmux--wait-for-pane
   (ghostel-tmux--spawn-controller session (ghostel-tmux--attach-args session))
   5.0))

;;;###autoload
(defun ghostel-tmux-new (session)
  "Create or attach to tmux SESSION via control mode.
An existing session is attached like in `ghostel-tmux-attach', as
`new-session -A' reaches it by a raw target; otherwise `tmux -CC
new-session -As' creates it."
  (interactive (list (read-string "tmux session name: " "main")))
  (when (string-empty-p session)
    (user-error "Ghostel-tmux: Empty session name"))
  (ghostel-tmux--check-spawn)
  (ghostel-tmux--wait-for-pane
   (ghostel-tmux--spawn-controller
    session
    (if (ghostel-tmux--session-exists-p session)
        (ghostel-tmux--attach-args session)
      ;; tmux expands `-s' as a format.
      (list "new-session" "-As" (ghostel-tmux--literal-format session))))
   5.0))

;;;###autoload
(defun ghostel-tmux-detach ()
  "Detach the current tmux session.
For sessions started via `ghostel-tmux-attach' or `ghostel-tmux-new',
kills the dedicated control-mode subprocess.  For sessions taken over
from an existing ghostel shell (takeover), sends `detach-client'
and lets tmux exit control mode cleanly so the underlying shell
continues running.  Pane buffers remain visible as read-only snapshots."
  (interactive)
  (let ((controller (ghostel-tmux--active-controller)))
    (unless (buffer-live-p controller)
      (user-error "Ghostel-tmux: No active controller for this buffer"))
    (with-current-buffer controller
      (let ((dedicated (derived-mode-p 'ghostel-tmux-control-mode)))
        (when (ghostel-tmux--controller-live-p)
          (ghostel-tmux--queue-command "detach-client")
          (setq ghostel-tmux--closing t)
          ;; Give tmux a brief moment to acknowledge the detach (and,
          ;; if another command was in flight, for the queue to reach
          ;; it) so its reply — and the DCS terminator in the
          ;; auto-takeover case — isn't lost to the upcoming
          ;; `delete-process'.
          (let ((proc (or ghostel-tmux--controller-process
                          ghostel--process)))
            (when (process-live-p proc)
              (accept-process-output proc 0.2))))
        (cond
         (dedicated
          ;; Dedicated controller buffer: kill its tmux -CC subprocess.
          (when ghostel-tmux--controller-process
            (delete-process ghostel-tmux--controller-process))
          (ghostel-tmux--seal-panes controller))
         (t
          ;; Auto-takeover host: tmux will exit control mode in response,
          ;; the native handler sees the DCS terminator, and
          ;; `ghostel-tmux--exit-control-mode' tears down pane state.
          nil))))))

;;;###autoload
(defun ghostel-tmux-next-pane ()
  "Switch to the next tmux pane in this session."
  (interactive)
  (ghostel-tmux--cycle-pane 1))

;;;###autoload
(defun ghostel-tmux-prev-pane ()
  "Switch to the previous tmux pane in this session."
  (interactive)
  (ghostel-tmux--cycle-pane -1))

(defvar ghostel-tmux--cycle-target nil
  "Pane buffer the last `ghostel-tmux--cycle-pane' asked tmux for.")

(defun ghostel-tmux--cycle-pane (direction)
  "Ask tmux to make the next pane in DIRECTION (+1 or -1) active.
The visible switch follows tmux's reports.  A repeated cycle steps on
from the pane it last asked for, whose report may still be on its way."
  (let* ((controller ghostel-tmux--controller-buffer)
         (panes (and (buffer-live-p controller)
                     (ghostel-tmux--pane-list controller)))
         (here (if (and (memq last-command
                              '(ghostel-tmux-next-pane ghostel-tmux-prev-pane))
                        (memq ghostel-tmux--cycle-target panes))
                   ghostel-tmux--cycle-target
                 (current-buffer)))
         (idx (cl-position here panes :test #'eq))
         (target (cond
                  ((null panes)
                   (user-error "Ghostel-tmux: No sibling panes"))
                  ((null idx) (car panes))
                  (t (nth (mod (+ idx direction) (length panes)) panes)))))
    (setq ghostel-tmux--cycle-target target)
    (ghostel-tmux--select-pane-buffer target)))

(defun ghostel-tmux--select-pane-buffer (pane-buf)
  "Ask tmux to switch to the pane of PANE-BUF, also across tmux windows.
The visible switch follows tmux's reports."
  (let ((pid (buffer-local-value 'ghostel-tmux--pane-id pane-buf))
        (wid (buffer-local-value 'ghostel-tmux--window-id pane-buf))
        (controller (buffer-local-value 'ghostel-tmux--controller-buffer
                                        pane-buf)))
    (when pid
      (when (and wid (buffer-live-p controller))
        (with-current-buffer controller
          (ghostel-tmux--cancel-zooming wid pid)))
      (ghostel-tmux--send-selection controller wid pid))))

;;;###autoload
(defun ghostel-tmux-switch-pane ()
  "Switch to a sibling pane via `completing-read'.
The visible switch follows tmux's reports.  The choices stay current
while the prompt is open, as tmux can rename a window at any time."
  (interactive)
  (let ((controller ghostel-tmux--controller-buffer))
    (cl-flet ((panes ()
                (and (buffer-live-p controller)
                     (ghostel-tmux--pane-list controller))))
      (unless (panes)
        (user-error "Ghostel-tmux: No sibling panes"))
      (when-let* ((pane (get-buffer
                         (completing-read
                          "Pane: "
                          (completion-table-dynamic
                           (lambda (_string) (mapcar #'buffer-name (panes))))
                          nil t))))
        (ghostel-tmux--select-pane-buffer pane)))))


;; One-shot snapshot of a single pane (no live connection)

;;;###autoload
(defun ghostel-tmux-capture (target)
  "Capture TARGET pane (e.g. \"session:window.pane\") into a fresh ghostel buffer.
The buffer renders the captured bytes through a VT engine but is not
connected to a live tmux session."
  (interactive
   (list (read-string "tmux target (e.g. mysession:0.0 or %3): "
                      (or (ghostel-tmux--prefix-target) ""))))
  (ghostel-tmux--load-module)
  (let* ((status nil)
         (output
          (with-output-to-string
            (with-current-buffer standard-output
              (setq status
                    (ghostel-tmux--process-file
                     "capture-pane" "-peJ"
                     "-S" (number-to-string (- ghostel-tmux-history-lines))
                     "-t" target))))))
    (unless (eql status 0)
      (user-error "Ghostel-tmux: capture-pane failed for %s" target))
    (when (string-empty-p output)
      (user-error "Ghostel-tmux: Empty capture for %s" target))
    (let ((buf (generate-new-buffer
                (format "*ghostel-tmux-capture:%s*" target))))
      (ghostel--init-buffer buf ghostel-tmux-default-rows
                            ghostel-tmux-default-cols)
      (with-current-buffer buf
        (ghostel-tmux-pane-mode 1)
        (ghostel-tmux--drop-tab-line)
        (setq ghostel--term-rows ghostel-tmux-default-rows
              ghostel--term-cols ghostel-tmux-default-cols
              ;; Swallow query replies (OSC/DA) — there is no live pane
              ;; to answer to.
              ghostel-pty-out-function #'ignore)
        ;; Rows are LF-separated and the VT engine does not synthesize CR.
        (ghostel--write-vt ghostel--term
                           (string-replace "\n" "\r\n"
                                           (string-trim-right output "\n+")))
        (setq ghostel-tmux--initialized t)
        (ghostel--invalidate))
      (pop-to-buffer buf))))

(provide 'ghostel-tmux)

;;; ghostel-tmux.el ends here
