;;; ghostel-tmux.el --- Tmux control mode for ghostel -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/ghostel-tmux
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (ghostel "0.61.0"))
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
;; `ghostel-control-mode-function'.
;; Pane data is fed into the existing native VT engine via
;; `ghostel--write-vt', and outbound keystrokes are routed through
;; `ghostel-pty-out-function' to the control-mode channel as
;; `send-keys -H' commands.

;;; Code:

(require 'ghostel)
(require 'cl-lib)
(require 'subr-x)
(require 'tab-line)

(declare-function ghostel--write-vt "ghostel-module" (term data))
(declare-function ghostel--write-pty "ghostel-module" (term data))

(defvar ghostel-control-mode-function)


;; Customization

(defgroup ghostel-tmux nil
  "Tmux control mode integration for ghostel."
  :group 'ghostel
  :prefix "ghostel-tmux-")

(defcustom ghostel-tmux-program "tmux"
  "The tmux executable to invoke for control mode."
  :type 'string
  :group 'ghostel-tmux)

(defvar ghostel-tmux-prefix-map)  ; Forward declaration for the defcustom :set function.

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
             (when (and old (not (equal old val)))
               (define-key ghostel-tmux-pane-mode-map (kbd old) nil)
               (define-key ghostel-tmux-prefix-map (kbd old) nil))
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

;; The "controller" is a hidden buffer that owns the tmux -CC subprocess.
;; It holds all parser state and dispatches notifications to pane buffers.

(defvar-local ghostel-tmux--controller-process nil
  "The tmux -CC subprocess hosted in this controller buffer.")

(defvar-local ghostel-tmux--parser-state 'preamble
  "Wire-format parser state.
One of: preamble, idle, notification, block.")

(defvar-local ghostel-tmux--line-buffer ""
  "Partial line accumulator across `process-filter' chunks.")

(defvar-local ghostel-tmux--block-lines nil
  "Reversed list of lines accumulated for the in-flight block payload.")

(defvar-local ghostel-tmux--block-tag nil
  "When inside a `%begin'/`%end' block, the parsed (TIME CMD FLAGS) tuple.
Currently only used by the parser to track that a block is open;
inspected for reset on terminator parsing.")

(defvar-local ghostel-tmux--command-queue nil
  "FIFO of pending tmux commands, each (CMD-STRING . CONTINUATION).
CONTINUATION is called with one argument BLOCK-OUTPUT (string) when the
matching `%end' arrives; or with nil when an `%error' is received.")

(defvar-local ghostel-tmux--command-in-flight nil
  "The (CMD-STRING . CONTINUATION) currently awaiting a block reply.")

(defvar-local ghostel-tmux--session-id nil
  "Numeric tmux session id this controller is attached to.")

(defvar-local ghostel-tmux--session-name nil
  "Human-readable tmux session name.")

(defvar-local ghostel-tmux--tmux-version nil
  "Version string reported by `display-message #{version}'.")

(defvar-local ghostel-tmux--panes nil
  "Hash table mapping integer pane-id (e.g. %3 -> 3) to its buffer.")

(defvar-local ghostel-tmux--windows nil
  "Hash table mapping integer window-id to a plist.
Its keys are :name, :panes, :layout, :active-pane (tmux's active pane
of that window) and :zooming (see `ghostel-tmux--ensure-pane-zoomed').")

(defvar-local ghostel-tmux--window-order nil
  "List of integer window-ids in display order.")

(defvar-local ghostel-tmux--current-window-id nil
  "Window id of the session's current window in tmux, or nil before known.")

(defvar-local ghostel-tmux--active-pane-id nil
  "Pane id of the active pane of the session's current window, or nil.")

(defvar-local ghostel-tmux--pending-follow-wid nil
  "Window id whose active pane we should follow once it materializes.
Set by `--on-session-window-changed' when tmux moves the session
to a window we haven't yet populated (typical for `new-window':
`%session-window-changed' fires before `%window-add'+list-windows
materialize the pane buffers).  Consumed by `--on-layout-change'.")

(defvar-local ghostel-tmux--viewer-buffer nil
  "Most-recently-displayed pane buffer, used to focus user actions.")

(defvar-local ghostel-tmux--exit-reason nil
  "Reason string from `%exit', for diagnostics.")

(defvar-local ghostel-tmux--closing nil
  "Non-nil once tmux is leaving control mode.
A command sent after `detach-client' or `%exit' would reach the host
shell of a takeover as typed input.")

(defvar-local ghostel-tmux--auto-original-name nil
  "Original buffer name of an auto-takeover host, saved on entry.
While in control mode the host buffer is renamed with a leading space
so it disappears from `buffer-list' and `switch-to-buffer' completion,
mirroring the experience of running tmux in a normal terminal where
the shell isn't a separate visible thing.  Restored on exit.")

(defvar ghostel-tmux--in-write-vt nil
  "Non-nil while feeding tmux `%output' bytes into a pane's VT engine.
Any `ghostel-pty-out-function' callbacks the engine produces in
response — OSC color-query replies, DA reports, cursor-position
reports — are suppressed while this is set.  Without that, a query
from an inner program (e.g. `bat' asking for terminal colors) would
round-trip through `send-keys', and by the time tmux delivered the
reply to the pane's stdin the program had usually exited; the bytes
then leaked into the next shell prompt as if typed.")

;; ghostel reports `\eP1000p ... \e\\' frames in a terminal's output to
;; `ghostel-tmux--control'.  Elisp only tracks whether *this buffer* is
;; currently the host of an active DCS — see `ghostel-tmux--dcs-active'
;; below.

(defvar-local ghostel-tmux--auto-host-window nil
  "When non-nil, the window to redirect tmux's active pane into.
Set on entry to control mode in an auto-takeover host so that the
materialized pane displaces the host's view in the same window —
matching the experience of running tmux in a normal terminal — on a
switch to another session, whose panes replace the ones shown, and when
the active pane goes away.")

(defvar-local ghostel-tmux--dcs-active nil
  "Non-nil when this buffer is currently inside a tmux DCS frame.
Canonical declaration; toggled by the `ghostel-tmux--dcs-{enter,exit}'
callbacks.  Pane buffers and dedicated controllers never set this —
`ghostel-tmux--dcs-enter' refuses to set it when
`ghostel-tmux-pane-mode' is on or the buffer's major mode is
`ghostel-tmux-control-mode'.")


;; Internal state — pane buffer

(defvar-local ghostel-tmux--controller-buffer nil
  "Pointer back to the controller buffer hosting the tmux process.")

(defvar-local ghostel-tmux--pane-id nil
  "Integer tmux pane id (e.g. 3 for %3) for this buffer.")

(defvar-local ghostel-tmux--window-id nil
  "Integer tmux window id this pane belongs to.")

(defvar-local ghostel-tmux--pane-name nil
  "Optional human label for this pane (window-name or index).")

(defvar-local ghostel-tmux--initialized nil
  "Non-nil once the initial capture-pane backfill has completed.
%output notifications received before this flag is set are dropped:
their effect is already contained in the capture snapshot.")

(defvar-local ghostel-tmux--bootstrap-pending nil
  "Non-nil while this pane's capture-pane backfill is queued.
A second `%layout-change' before the replies must not queue another
pair: that would render the scrollback twice.")


;; Octal escape decoding

;; tmux's `%output' payload escapes 0x00-0x1F (and DEL) as 3-digit octal
;; "\NNN" — for example ESC arrives as the four ASCII bytes "\033", CR as
;; "\015", LF as "\012".  We must decode these back to raw bytes before
;; feeding them to the VT engine.

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
;;   notification -- accumulating a single `%foo ...' line.
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
  (when (memq ghostel-tmux--parser-state '(idle notification block))
    (let ((idx 0)
          (ghostel-tmux--in-feed t))
      ;; `ghostel-tmux--handle-line' may reset
      ;; `ghostel-tmux--line-buffer' under our feet — re-check the
      ;; buffer length before every match so a stale IDX can't index
      ;; past the emptied accumulator.
      (while (and (<= idx (length ghostel-tmux--line-buffer))
                  (string-match "\r?\n" ghostel-tmux--line-buffer idx))
        (let ((line (substring ghostel-tmux--line-buffer
                               idx (match-beginning 0))))
          (setq idx (match-end 0))
          (ghostel-tmux--handle-line line)))
      (setq ghostel-tmux--line-buffer
            (if (<= idx (length ghostel-tmux--line-buffer))
                (substring ghostel-tmux--line-buffer idx)
              "")))))

(defun ghostel-tmux--handle-line (line)
  "Dispatch a single LINE (no trailing CRLF) through the state machine."
  (pcase ghostel-tmux--parser-state
    ('idle (ghostel-tmux--handle-idle-line line))
    ('block (ghostel-tmux--handle-block-line line))
    ('notification
     ;; Single-line notification mode is handled inline in the idle state.
     (setq ghostel-tmux--parser-state 'idle)
     (ghostel-tmux--handle-idle-line line))
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
             (cmd ghostel-tmux--command-in-flight))
        (setq ghostel-tmux--block-lines nil
              ghostel-tmux--block-tag nil
              ghostel-tmux--parser-state 'idle
              ghostel-tmux--command-in-flight nil)
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
  (let ((controller (or ghostel-tmux--controller-buffer
                        (and (or (derived-mode-p 'ghostel-tmux-control-mode)
                                 ghostel-tmux--dcs-active)
                             (current-buffer)))))
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
     (match-string 2 line)))
   ((string-match "\\`%session-window-changed \\$\\([0-9]+\\) @\\([0-9]+\\)\\'"
                  line)
    (ghostel-tmux--on-session-window-changed
     (string-to-number (match-string 1 line))
     (string-to-number (match-string 2 line))))
   ;; tmux >= 2.5 prefixes the new name with the session id; older
   ;; versions emit the name alone.
   ((string-match "\\`%session-renamed \\(?:\\$\\([0-9]+\\) \\)?\\(.+\\)\\'" line)
    (let ((sid (match-string 1 line))
          (name (match-string 2 line)))
      ;; Sent for every session on the server.
      (when (or (null sid)
                (eql (string-to-number sid) ghostel-tmux--session-id))
        (ghostel-tmux--on-session-renamed name))))
   ((string= "%sessions-changed" line)
    nil)                                   ; treat as advisory
   ((string-match "\\`%window-add @\\([0-9]+\\)\\'" line)
    (ghostel-tmux--on-window-add
     (string-to-number (match-string 1 line))))
   ((string-match "\\`%\\(?:unlinked-\\)?window-close @\\([0-9]+\\)\\'" line)
    ;; tmux 3.6 emits `%unlinked-window-close @WID' for the client whose
    ;; session no longer contains the window — which is the only client
    ;; in the single-attach case — so the pane buffers stay orphaned
    ;; unless we treat both forms as the close signal.
    (ghostel-tmux--on-window-close
     (string-to-number (match-string 1 line))))
   ((string-match "\\`%window-renamed @\\([0-9]+\\) \\(.*\\)\\'" line)
    (ghostel-tmux--on-window-renamed
     (string-to-number (match-string 1 line))
     (match-string 2 line)))
   ((string-match
     "\\`%window-pane-changed @\\([0-9]+\\) %\\([0-9]+\\)\\'" line)
    (ghostel-tmux--on-window-pane-changed
     (string-to-number (match-string 1 line))
     (string-to-number (match-string 2 line))))
   ((string-match
     ;; tmux 3.6 always emits 4 fields:
     ;;   %layout-change @WID LAYOUT VISIBLE_LAYOUT RAW_FLAGS
     ;; but `#{window_raw_flags}' can expand to "" for a window with no
     ;; printable flags, producing a trailing space we must tolerate.
     ;; Older tmux only emits the LAYOUT field.
     "\\`%layout-change @\\([0-9]+\\) \\([^ ]+\\)\\(?: \\([^ ]*\\)\\)?\\(?: [^ ]*\\)\\{0,2\\} *\\'"
     line)
    (ghostel-tmux--on-layout-change
     (string-to-number (match-string 1 line))
     (match-string 2 line)
     (match-string 3 line)))
   ((string-match "\\`%exit\\(?: \\(.*\\)\\)?\\'" line)
    (ghostel-tmux--on-exit (match-string 1 line)))
   (t
    ;; Unknown notification — log and continue.
    nil)))


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
          (setq ghostel-tmux--window-id window-id)
          (when window-name
            (setq ghostel-tmux--pane-name window-name)))
        existing)
       (t
        (let* ((session-name ghostel-tmux--session-name)
               (name (funcall ghostel-tmux-buffer-name-function
                              (or session-name "?")
                              window-name
                              pane-id))
               (buf (generate-new-buffer name)))
          (puthash pane-id buf ghostel-tmux--panes)
          (ghostel--init-buffer buf ghostel-tmux-default-rows
                                ghostel-tmux-default-cols)
          (with-current-buffer buf
            (ghostel-tmux-pane-mode 1)
            (setq ghostel-tmux--controller-buffer controller
                  ghostel-tmux--pane-id pane-id
                  ghostel-tmux--window-id window-id
                  ghostel-tmux--pane-name window-name
                  ghostel--term-rows ghostel-tmux-default-rows
                  ghostel--term-cols ghostel-tmux-default-cols
                  ghostel-pty-out-function
                  (apply-partially #'ghostel-tmux--send-bytes-from-pane
                                   controller pane-id)
                  ghostel-resize-function #'ghostel-tmux--resize-pane))
          buf))))))

(defun ghostel-tmux--remove-pane (controller pane-id)
  "Drop pane PANE-ID from CONTROLLER and kill its buffer."
  (with-current-buffer controller
    (let ((buf (gethash pane-id ghostel-tmux--panes)))
      (when buf
        (remhash pane-id ghostel-tmux--panes)
        (when (buffer-live-p buf)
          (when (eql pane-id ghostel-tmux--active-pane-id)
            (ghostel-tmux--keep-lost-pane-window buf))
          (with-current-buffer buf
            (setq ghostel-pty-out-function nil))
          (kill-buffer buf))))))

(defun ghostel-tmux--keep-lost-pane-window (pane-buf)
  "Keep the window of PANE-BUF, tmux's active pane, which is going away.
tmux reports the next active pane after the pane is gone, and sends no
report when its active pane was another one; asking settles both."
  (when-let* ((window (get-buffer-window pane-buf 'visible)))
    (setq ghostel-tmux--auto-host-window window)
    (ghostel-tmux--queue-command "display-message -p '#{pane_id}'"
                                 #'ghostel-tmux--receive-active-pane)))

(defun ghostel-tmux--receive-active-pane (output)
  "Follow the active pane in OUTPUT into a window kept for a lost pane.
A report may have done that already.  A pane without a buffer yet leaves
the window to the rows on their way; a failed query gives it up."
  (when ghostel-tmux--auto-host-window
    (cond
     ((null output)
      (setq ghostel-tmux--auto-host-window nil))
     ((string-match "%\\([0-9]+\\)" output)
      (let ((pid (string-to-number (match-string 1 output))))
        (when (and ghostel-tmux--panes (gethash pid ghostel-tmux--panes))
          (setq ghostel-tmux--active-pane-id pid)
          (ghostel-tmux--follow-active-pane pid)))))))

(defun ghostel-tmux--pane-list (controller)
  "Return the live pane buffers under CONTROLLER, in window+pane order."
  (with-current-buffer controller
    (let ((result nil))
      (dolist (wid (or ghostel-tmux--window-order
                       (and ghostel-tmux--windows
                            (hash-table-keys ghostel-tmux--windows))))
        (maphash (lambda (_pid buf)
                   (when (and (buffer-live-p buf)
                              (eq (buffer-local-value
                                   'ghostel-tmux--window-id buf)
                                  wid))
                     (push buf result)))
                 ghostel-tmux--panes))
      (nreverse result))))


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
      (ghostel-tmux--initial-bootstrap))
     (changed
      ;; Switching sessions resets pane state.  Wipe and reload; the
      ;; Emacs window goes on showing the new session.
      (setq ghostel-tmux--auto-host-window
            (or (ghostel-tmux--pane-window) ghostel-tmux--auto-host-window))
      (ghostel-tmux--wipe-panes)
      (ghostel-tmux--bootstrap-windows)))))

(defconst ghostel-tmux--window-row-format
  "#{window_id}\\t#{pane_id}\\t#{window_active}\\t#{window_name}\\t#{window_layout}\\t#{window_visible_layout}"
  "The `list-windows -F' format that `--receive-window-rows' parses.")

(defun ghostel-tmux--on-window-add (wid)
  "Handle `%window-add @WID': fetch its layout and create panes."
  (unless (gethash wid ghostel-tmux--windows)
    (puthash wid (list :name nil :layout nil) ghostel-tmux--windows)
    (unless (memq wid ghostel-tmux--window-order)
      (setq ghostel-tmux--window-order
            (append ghostel-tmux--window-order (list wid)))))
  (ghostel-tmux--queue-command
   (format "list-windows -F \"%s\"" ghostel-tmux--window-row-format)
   (lambda (output)
     (when output
       (ghostel-tmux--receive-window-rows output)))))

(defun ghostel-tmux--on-window-close (wid)
  "Handle `%window-close @WID': drop the window and its panes."
  (let ((win (gethash wid ghostel-tmux--windows)))
    (when win
      (let ((panes (plist-get win :panes)))
        (dolist (pid panes)
          (ghostel-tmux--remove-pane (current-buffer) pid))))
    (remhash wid ghostel-tmux--windows))
  (setq ghostel-tmux--window-order
        (delq wid ghostel-tmux--window-order)))

(defun ghostel-tmux--on-session-renamed (name)
  "Handle `%session-renamed': relabel the session and its pane buffers as NAME."
  (setq ghostel-tmux--session-name name)
  (when ghostel-tmux--windows
    (maphash
     (lambda (_wid win)
       (let ((window-name (plist-get win :name)))
         (dolist (pid (plist-get win :panes))
           (let ((buf (and ghostel-tmux--panes
                           (gethash pid ghostel-tmux--panes))))
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (rename-buffer (funcall ghostel-tmux-buffer-name-function
                                         name window-name pid)
                                t)
                 (force-mode-line-update)))))))
     ghostel-tmux--windows))
  (ghostel-tmux--refresh-tab-line))

(defun ghostel-tmux--on-window-renamed (wid name)
  "Handle `%window-renamed @WID NAME'."
  (let ((win (gethash wid ghostel-tmux--windows))
        (session-name ghostel-tmux--session-name))
    (when win
      (puthash wid (plist-put win :name name) ghostel-tmux--windows)
      (dolist (pid (plist-get win :panes))
        (let ((buf (gethash pid ghostel-tmux--panes)))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (setq ghostel-tmux--pane-name name)
              (rename-buffer (funcall ghostel-tmux-buffer-name-function
                                      session-name
                                      name pid)
                             t)
              (force-mode-line-update)))))))
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
  (let* ((new-buf (and new-pid ghostel-tmux--panes
                       (gethash new-pid ghostel-tmux--panes)))
         (controller (current-buffer)))
    (when (buffer-live-p new-buf)
      (setq ghostel-tmux--viewer-buffer new-buf)
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
          (with-current-buffer controller
            (ghostel-tmux--display-pane-in-window new-buf target-window)))))))

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
    (ghostel-tmux--follow-active-pane pid)))

(defun ghostel-tmux--on-session-window-changed (sid wid)
  "Handle `%session-window-changed $SID @WID': follow the new window.
Sent for every session on the server; other sessions are ignored.
Follow the window's recorded active pane, or select its first pane when
none is recorded.  A window without panes yet, or whose recorded pane is
still moving in (`join-pane'), waits for `--on-layout-change'."
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
        (unless known
          (ghostel-tmux--queue-command (format "select-pane -t %%%d" target)))
        (ghostel-tmux--follow-active-pane target))
       (t
        (setq ghostel-tmux--pending-follow-wid wid))))))

(defun ghostel-tmux--on-layout-change (wid layout &optional visible-layout)
  "Handle `%layout-change @WID LAYOUT VISIBLE-LAYOUT' — reconcile panes.
LAYOUT has every pane of the window, at its unzoomed size in a zoomed
window; VISIBLE-LAYOUT has the panes on screen at their real size."
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
      (setq entry (plist-put entry :layout layout))
      (puthash wid entry ghostel-tmux--windows))
    (unless (memq wid ghostel-tmux--window-order)
      (setq ghostel-tmux--window-order
            (append ghostel-tmux--window-order (list wid))))
    (dolist (pid old-ids)
      (unless (memq pid pane-ids)
        (ghostel-tmux--remove-pane controller pid)))
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
    ;; In auto-takeover mode, redirect the host's window to tmux's active
    ;; pane so the user keeps seeing tmux in "their" buffer.  A layout
    ;; alone does not say which pane that is: wait for the window's rows.
    (let ((recorded (plist-get (gethash wid ghostel-tmux--windows)
                               :active-pane)))
      (when (and ghostel-tmux--auto-host-window
                 (window-live-p ghostel-tmux--auto-host-window)
                 (eql wid ghostel-tmux--current-window-id)
                 (memq recorded pane-ids))
        (let ((pane (gethash recorded ghostel-tmux--panes))
              (target-window ghostel-tmux--auto-host-window))
          (setq ghostel-tmux--auto-host-window nil
                ghostel-tmux--active-pane-id recorded)
          (when (buffer-live-p pane)
            (ghostel-tmux--display-pane-in-window pane target-window)))))
    ;; Deferred follow: `%session-window-changed' fired for this WID
    ;; before its panes existed (typical for new-window — the
    ;; session-window-changed arrives before window-add + list-windows
    ;; materialize the layout).  Follow once those rows have told which
    ;; pane tmux has active.
    (let ((recorded (plist-get (gethash wid ghostel-tmux--windows)
                               :active-pane)))
      (when (and (eql ghostel-tmux--pending-follow-wid wid)
                 (memq recorded pane-ids))
        (setq ghostel-tmux--pending-follow-wid nil
              ghostel-tmux--active-pane-id recorded)
        (ghostel-tmux--follow-active-pane recorded)))
    ;; Also catch the within-window race: `%window-pane-changed'
    ;; arrived before `%layout-change' created the pane buffer.
    (when (and ghostel-tmux--active-pane-id
               (memq ghostel-tmux--active-pane-id pane-ids))
      (ghostel-tmux--follow-active-pane ghostel-tmux--active-pane-id))
    (ghostel-tmux--refresh-tab-line)))

(defun ghostel-tmux--on-exit (reason)
  "Handle `%exit' from tmux, recording REASON if given."
  (setq ghostel-tmux--exit-reason (or reason "")
        ghostel-tmux--closing t
        ghostel-tmux--command-queue nil)
  (message "Ghostel-tmux: tmux exited%s"
           (if (and reason (not (string-empty-p reason)))
               (format ": %s" reason)
             "")))


;; Bootstrap sequence

(defun ghostel-tmux--initial-bootstrap ()
  "Initial command sequence after `%session-changed' arrives."
  (ghostel-tmux--queue-command
   "display-message -p \"#{version}\""
   (lambda (output)
     (when output
       (setq ghostel-tmux--tmux-version (string-trim output)))))
  (ghostel-tmux--bootstrap-windows))

(defun ghostel-tmux--bootstrap-windows ()
  "Fetch the current session's window list and seed pane buffers."
  (ghostel-tmux--queue-command
   (format "list-windows -F \"%s\"" ghostel-tmux--window-row-format)
   (lambda (output)
     (when output
       (ghostel-tmux--receive-window-rows output t)))))

(defun ghostel-tmux--parse-window-row (line)
  "Parse `list-windows' LINE, or return nil for any other line.
The result is (WID ACTIVE-PANE CURRENT NAME LAYOUT VISIBLE-LAYOUT)."
  (when (string-match
         "\\`@\\([0-9]+\\)\t%\\([0-9]+\\)\t\\([01]\\)\t\\(.*\\)\t\\(.+\\)\t\\(.*\\)\\'" line)
    (list (string-to-number (match-string 1 line))
          (string-to-number (match-string 2 line))
          (equal (match-string 3 line) "1")
          (match-string 4 line)
          (match-string 5 line)
          (match-string 6 line))))

(defun ghostel-tmux--receive-window-rows (output &optional seed)
  "Parse `list-windows' OUTPUT and create/update windows + panes.
Each row is `@WID<TAB>%ACTIVE-PANE<TAB>CURRENT<TAB>WINDOW-NAME<TAB>LAYOUT
<TAB>VISIBLE-LAYOUT'.  Using TAB as the delimiter (rather than space)
lets us round-trip window names that contain spaces — common with
tmux's automatic-rename feature.
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
      (unless (memq wid ghostel-tmux--window-order)
        (setq ghostel-tmux--window-order
              (append ghostel-tmux--window-order (list wid))))
      (ghostel-tmux--on-layout-change wid layout visible))))

(defun ghostel-tmux--bootstrap-pane (controller pane-id)
  "Issue one capture-pane on CONTROLLER to backfill PANE-ID.
The capture spans scrollback and the visible screen (a separate
history capture with an empty scrollback repeats the first screen
row).  `-C' octal-escapes control bytes: %begin/%end payloads are
otherwise raw, and a raw ESC would end the control-mode DCS."
  (with-current-buffer controller
    (ghostel-tmux--queue-command
     (format "capture-pane -peJC -t %%%d -S -%d"
             pane-id ghostel-tmux-history-lines)
     (apply-partially #'ghostel-tmux--receive-pane-history
                      controller pane-id))))

(defun ghostel-tmux--receive-pane-history (controller pane-id output)
  "Apply the capture-pane OUTPUT to PANE-ID's terminal under CONTROLLER.
Rows are LF-separated and the VT engine does not synthesize CR, so they
are fed as CRLF; trailing blank rows are dropped so the cursor lands
after the last content line."
  (let ((buf (with-current-buffer controller
               (gethash pane-id ghostel-tmux--panes))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when output
          (let ((data (ghostel-tmux--decode-output
                       (string-replace "\n" "\r\n"
                                       (string-trim-right output "\n+"))))
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
    (when ghostel-tmux--panes
      (maphash (lambda (pid _buf)
                 (ghostel-tmux--remove-pane controller pid))
               ghostel-tmux--panes))
    (clrhash ghostel-tmux--panes)
    (when ghostel-tmux--windows
      (clrhash ghostel-tmux--windows))
    (setq ghostel-tmux--window-order nil
          ghostel-tmux--current-window-id nil
          ghostel-tmux--pending-follow-wid nil
          ghostel-tmux--viewer-buffer nil)))


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
The PTY between Emacs and the tmux controller has a finite line
buffer; large pastes must be split.  At 3 chars/byte plus the
`send-keys -t %%N -H ' header, 512 source bytes ≈ 1.6 KB per line,
well under the typical 4 KB line discipline limit.")

(defun ghostel-tmux--send-bytes-from-pane (controller pane-id data)
  "Send DATA bytes to tmux PANE-ID via CONTROLLER as `send-keys -H'.
Splits long DATA into multiple `send-keys' commands so a single line
never exceeds the controller PTY's input buffer (which would silently
truncate large pastes).  Each chunk goes through the command queue —
see the controller-output-channel commentary for why nothing may
bypass it.

Bytes generated as a side-effect of feeding `%output' (terminal
responses to OSC/DA/cursor queries) are dropped — see
`ghostel-tmux--in-write-vt' for why."
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
                     (slice (substring unibytes i end))
                     (hex (mapconcat (lambda (b) (format "%02x" b))
                                     slice " ")))
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
controller buffer itself was killed before the process died."
  (unless (process-live-p proc)
    (let ((buf (process-buffer proc)))
      (ghostel-tmux--seal-panes buf)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (ghostel-tmux--refresh-tab-line)))
      (message "Ghostel-tmux: %s" (string-trim event)))))


;; Modes

(defun ghostel-tmux--cmd (command)
  "Run tmux command-mode COMMAND through the active controller.
Routed through `ghostel-tmux--queue-command' so a `%error' reply from
tmux is captured and surfaced to the user instead of being eaten by
whichever block happens to be in flight at the time."
  (let ((controller (or ghostel-tmux--controller-buffer
                        (and (derived-mode-p 'ghostel-tmux-control-mode)
                             (current-buffer))
                        (and ghostel-tmux--dcs-active
                             (current-buffer)))))
    (unless (buffer-live-p controller)
      (user-error "Ghostel-tmux: No active controller"))
    (with-current-buffer controller
      (unless (ghostel-tmux--controller-live-p)
        (user-error "Ghostel-tmux: Controller is not connected"))
      (ghostel-tmux--queue-command
       command
       (lambda (output)
         (when (null output)
           ;; tmux replied with `%error' — surface to the user.
           (message "Ghostel-tmux: command failed: %s" command)))))))

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

(defvar ghostel-tmux-prefix-map
  (let ((map (make-sparse-keymap)))
    map)
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

(defun ghostel-tmux--cycle-window (direction)
  "Ask tmux to make the next/previous window active.
DIRECTION is +1 (next) or -1 (previous).  The visible switch
happens reactively in `--on-session-window-changed' once tmux acks
the `select-window'."
  (let ((controller ghostel-tmux--controller-buffer)
        (cur-wid ghostel-tmux--window-id))
    (unless (and controller (buffer-live-p controller) cur-wid)
      (user-error "Ghostel-tmux: No controller/window context"))
    (let* ((order (with-current-buffer controller
                    ghostel-tmux--window-order))
           (idx (cl-position cur-wid order)))
      (when (and idx (> (length order) 1))
        (let ((next-wid (nth (mod (+ idx direction) (length order))
                             order)))
          (ghostel-tmux--cmd
           (ghostel-tmux--select-window-command controller next-wid)))))))

(defun ghostel-tmux--prefix-next-window ()
  "Switch to the next tmux window and display its active pane."
  (interactive)
  (ghostel-tmux--cycle-window 1))
(define-key ghostel-tmux-prefix-map (kbd "n") #'ghostel-tmux--prefix-next-window)

(defun ghostel-tmux--prefix-prev-window ()
  "Switch to the previous tmux window and display its active pane."
  (interactive)
  (ghostel-tmux--cycle-window -1))
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

(ghostel-tmux--define-prefix-cmd zoom "z"
  (lambda (target) (format "resize-pane -Z -t %s" target))
  "Toggle zoom on the current pane.")

(ghostel-tmux--define-prefix-cmd cmd ":"
  (lambda (_target)
    (read-string "tmux command: "))
  "Prompt for an arbitrary tmux command and send it.")

;; Number keys: select tmux window N in the current session.  We pin
;; the target to `$SID:N' so multi-session controllers don't switch
;; the wrong session's window.
(dotimes (n 10)
  (let ((key (number-to-string n))
        (idx n))
    (define-key ghostel-tmux-prefix-map (kbd key)
      (let ((fn (intern (format "ghostel-tmux--prefix-window-%d" idx))))
        (defalias fn
          (lambda ()
            (interactive)
            (let* ((controller ghostel-tmux--controller-buffer)
                   (sid (and (buffer-live-p controller)
                             (with-current-buffer controller
                               ghostel-tmux--session-id))))
              (ghostel-tmux--cmd
               (if sid
                   (format "select-window -t $%d:%d" sid idx)
                 (format "select-window -t :%d" idx)))))
          (format "Switch to window %d in the current tmux session." idx))
        fn))))

;; `<prefix> d': detach (matches tmux's default).
(define-key ghostel-tmux-prefix-map (kbd "d") #'ghostel-tmux-detach)

;; `<prefix> ?': describe the prefix map.
(define-key ghostel-tmux-prefix-map (kbd "?")
            (lambda ()
              (interactive)
              (describe-keymap 'ghostel-tmux-prefix-map)))

(defun ghostel-tmux--prefix-self-insert ()
  "Send the tmux prefix key itself to the underlying program.
Bound under `ghostel-tmux-prefix-map' as `<prefix> <prefix>', matching
tmux's convention.  Only meaningful when `ghostel-tmux-prefix-key'
resolves to a single control byte (`C-<letter>')."
  (interactive)
  (let* ((seq (kbd ghostel-tmux-prefix-key))
         (byte (and (= (length seq) 1)
                    (let ((e (aref seq 0)))
                      (and (integerp e) (<= 0 e 31) e)))))
    (if byte
        (ghostel--send-string (string byte))
      (message "Ghostel-tmux: %S is not a control byte; cannot send raw"
               ghostel-tmux-prefix-key))))

(defvar ghostel-tmux-pane-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") #'ghostel-tmux-next-pane)
    (define-key map (kbd "C-c C-p") #'ghostel-tmux-prev-pane)
    (define-key map (kbd "C-c C-s") #'ghostel-tmux-switch-pane)
    (define-key map (kbd "C-c C-d") #'ghostel-tmux-detach)
    map)
  "Keymap for `ghostel-tmux-pane-mode'.
Sits above the local input-mode map via the standard minor-mode
dispatch, so the tmux prefix and `C-c' navigation compose with
ghostel's semi-char, line, and emacs-mode input maps without
replacing their bindings.  In char mode the emulation-alist
\(`ghostel-char-mode-map') sits above minor-mode keymaps, so the
prefix is intentionally inert and the underlying program receives
the keystroke raw.")

(defun ghostel-tmux--install-prefix-key (key)
  "Install KEY as the tmux prefix in `ghostel-tmux-pane-mode-map'.
Also binds `<prefix> <prefix>' in the prefix sub-map to send the
prefix byte raw to the underlying program."
  (define-key ghostel-tmux-pane-mode-map (kbd key) ghostel-tmux-prefix-map)
  (define-key ghostel-tmux-prefix-map (kbd key)
              #'ghostel-tmux--prefix-self-insert))

(ghostel-tmux--install-prefix-key ghostel-tmux-prefix-key)

(define-minor-mode ghostel-tmux-pane-mode
  "Minor mode making this ghostel buffer act as a tmux pane.
Activated when the controller materializes a pane buffer.  Composes
with the active ghostel input mode (semi-char, line, or
emacs-mode) — the tmux prefix and `C-c' navigation are added
without replacing the terminal's key bindings.  In char mode the
prefix is intentionally inert so a TUI program inside the pane
receives the keystroke raw."
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
  "Refresh `tab-line-mode' display in every pane buffer of this controller.
Called when the pane set changes (window-add, layout-change,
window-renamed, window-close, sentinel)."
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
  "When a pane buffer dies, evict it from the controller."
  (let ((controller ghostel-tmux--controller-buffer)
        (pid ghostel-tmux--pane-id))
    (when (and pid (buffer-live-p controller))
      (with-current-buffer controller
        (when ghostel-tmux--panes
          (remhash pid ghostel-tmux--panes))))))


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

(defun ghostel-tmux--control (event data)
  "Handle control-mode EVENT with DATA in the current terminal buffer.
The `ghostel-control-mode-function' of `ghostel-tmux-takeover-mode'."
  (pcase event
    ('enter
     (unless (ghostel-tmux--dcs-enter)
       (message (concat "Ghostel-tmux: Ignoring tmux control-mode output in %s; "
                        "it stays hidden until tmux detaches "
                        "(M-x ghostel-tmux-reset to force)")
                (buffer-name))))
    ('data (ghostel-tmux--dcs-data data))
    ('exit (ghostel-tmux--dcs-exit))))

;;;###autoload
(define-minor-mode ghostel-tmux-takeover-mode
  "Turn `tmux -CC' typed in a ghostel shell into pane buffers.
The shell's buffer hides itself while tmux is attached and comes back
when tmux detaches."
  :global t
  :group 'ghostel-tmux
  (when (and ghostel-tmux-takeover-mode
             (not (boundp 'ghostel-control-mode-function)))
    (setq ghostel-tmux-takeover-mode nil)
    (user-error "This ghostel has no `ghostel-control-mode-function'"))
  (setq ghostel-control-mode-function
        (and ghostel-tmux-takeover-mode #'ghostel-tmux--control)))

(defun ghostel-tmux--dcs-enter ()
  "Handle a tmux control-mode DCS opening in the current buffer.
Enters control mode unless we're already inside a tmux-related buffer
where DCS doesn't make sense (a pane buffer with `%output' that happens
to contain `\\eP1000p', or the dedicated controller's hidden buffer).
Returns non-nil when control
mode was entered or is already active in this buffer, nil when the
takeover is declined."
  (cond
   (ghostel-tmux--dcs-active t)
   ((and (not (bound-and-true-p ghostel-tmux-pane-mode))
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
plain shell."
  (when ghostel-tmux--dcs-active
    (setq ghostel-tmux--dcs-active nil)
    (ghostel-tmux--exit-control-mode (current-buffer))))

(defun ghostel-tmux--enter-control-mode (buf)
  "Take over BUF (a ghostel buffer) as a tmux controller.
Initializes tmux controller state in-place: commands ride the host
terminal's PTY (via `ghostel-tmux--controller-send'), and pane buffers
are created as the parser sees notifications.  The host buffer is
also renamed with a leading space so it doesn't appear in the
user-visible buffer list — pane buffers replace it as the visible
front-end."
  (with-current-buffer buf
    (let ((original-name (buffer-name)))
      (setq ghostel-tmux--auto-original-name original-name)
      (rename-buffer (format " *ghostel-tmux-host:%s*" original-name) t))
    (add-hook 'ghostel-exit-functions #'ghostel-tmux--host-exited nil t)
    (add-hook 'kill-buffer-hook #'ghostel-tmux--host-killed nil t)
    ;; `ghostel-tmux--controller-process' stays nil: the control stream
    ;; rides the host PTY, which `ghostel-tmux--controller-send' reaches
    ;; through `ghostel--write-pty'.
    (setq ghostel-tmux--controller-process nil
          ghostel-tmux--session-name (or ghostel-tmux--auto-original-name
                                          (buffer-name))
          ghostel-tmux--parser-state 'idle
          ghostel-tmux--line-buffer ""
          ghostel-tmux--block-lines nil
          ghostel-tmux--block-tag nil
          ghostel-tmux--command-queue nil
          ghostel-tmux--command-in-flight nil
          ghostel-tmux--closing nil
          ghostel-tmux--session-id nil
          ghostel-tmux--panes (make-hash-table)
          ghostel-tmux--windows (make-hash-table)
          ghostel-tmux--window-order nil
          ghostel-tmux--current-window-id nil
          ghostel-tmux--pending-follow-wid nil
          ghostel-tmux--active-pane-id nil
          ghostel-tmux--viewer-buffer nil
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
    (setq ghostel-tmux--controller-process nil
          ghostel-tmux--parser-state 'preamble
          ghostel-tmux--line-buffer ""
          ghostel-tmux--block-lines nil
          ghostel-tmux--block-tag nil
          ghostel-tmux--command-queue nil
          ghostel-tmux--command-in-flight nil
          ghostel-tmux--panes nil
          ghostel-tmux--windows nil
          ghostel-tmux--window-order nil
          ghostel-tmux--current-window-id nil
          ghostel-tmux--pending-follow-wid nil
          ghostel-tmux--active-pane-id nil)
    (remove-hook 'ghostel-exit-functions #'ghostel-tmux--host-exited t)
    (remove-hook 'kill-buffer-hook #'ghostel-tmux--host-killed t)
    ;; Restore the host buffer's original (visible) name, if we renamed
    ;; it during entry to hide it from the user-visible buffer list.
    (when ghostel-tmux--auto-original-name
      (let ((original ghostel-tmux--auto-original-name))
        (setq ghostel-tmux--auto-original-name nil)
        (ignore-errors (rename-buffer original t))))
    (message "Ghostel-tmux: Exited control mode (buffer %s)"
             (buffer-name buf))))

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

(defun ghostel-tmux--host-killed ()
  "Seal the panes of this auto-takeover host, whose buffer is being killed.
`ghostel-exit-functions' does not run for a buffer that is gone."
  (ghostel-tmux--seal-panes (current-buffer)))


;; Public commands

(defun ghostel-tmux--read-session ()
  "Prompt for a tmux session name and return it.
A non-zero exit (no server running) counts as no sessions."
  (let* ((status nil)
         (output (with-output-to-string
                   (with-current-buffer standard-output
                     (setq status
                           (process-file ghostel-tmux-program nil
                                         (list t nil) nil
                                         "list-sessions" "-F"
                                         "#{session_name}")))))
         (sessions (and (eql status 0) (split-string output "\n" t)))
         (default (car sessions)))
    (cond
     ((null sessions)
      (user-error "Ghostel-tmux: No tmux sessions found"))
     ((= (length sessions) 1) (car sessions))
     (t (completing-read "tmux session: " sessions nil t nil nil default)))))

(defun ghostel-tmux--spawn-controller (session tmux-args)
  "Spawn a tmux -CC subprocess as a controller buffer.
SESSION is the user-visible session label.  TMUX-ARGS is the list
of arguments after \"tmux -CC\".  Returns the controller buffer."
  (let ((control-buf (generate-new-buffer
                      (format " *ghostel-tmux-control:%s*" session))))
    (with-current-buffer control-buf
      (ghostel-tmux-control-mode)
      (let* ((proc (make-process
                    :name (format "ghostel-tmux-%s" session)
                    :buffer control-buf
                    :command (cons ghostel-tmux-program (cons "-CC" tmux-args))
                    :coding 'binary
                    :connection-type 'pty
                    :noquery t
                    :filter #'ghostel-tmux--filter
                    :sentinel #'ghostel-tmux--sentinel)))
        (set-process-query-on-exit-flag proc nil)
        (setq ghostel-tmux--controller-process proc
              ghostel-tmux--session-name session)))
    control-buf))

(defun ghostel-tmux--apply-pane-size (pane-buf window)
  "Fit PANE-BUF's terminal to WINDOW showing it and tell tmux about the size.
Splitting and tab-line switches both hit this path, so a pane that
was created at default geometry gets corrected the moment it becomes
visible in an Emacs window.  The size is reported even when the grid
already fits, because tmux's client size may still be that of another
pane's window.  tmux is asked to switch to the pane, and when its
window has more than one pane, to zoom it to the full client size — without
that, splitting (`C-b \"' / `C-b %%') leaves the user looking at half
a pane in a full-height Emacs window."
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
has: answering its report with a selection would chase a newer
selection forever."
  (unless (eql pane-id ghostel-tmux--active-pane-id)
    (ghostel-tmux--send-selection (current-buffer) window-id pane-id)
    (setq ghostel-tmux--current-window-id window-id
          ghostel-tmux--active-pane-id pane-id)))

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
      (switch-to-buffer pane-buf))
    (ghostel-tmux--apply-pane-size pane-buf window)))

(defun ghostel-tmux--on-window-buffer-change (window)
  "Resize the pane terminal when this buffer becomes WINDOW's content.
Hook for `window-buffer-change-functions': covers tab-line clicks and
any other path that swaps the buffer into a window without going
through `ghostel-tmux--display-pane-in-window'."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (buffer-live-p ghostel-tmux--controller-buffer))
    (ghostel-tmux--apply-pane-size (current-buffer) window)))

(defun ghostel-tmux--wait-for-pane (control-buf timeout)
  "Block up to TIMEOUT seconds for CONTROL-BUF to materialize a pane.
Switches the selected window to tmux's active pane when one appears,
or to the first pane while the active one is unknown."
  (let ((target-window (selected-window))
        (deadline (+ (float-time) timeout)))
    (catch 'done
      (while (< (float-time) deadline)
        (let ((panes (ghostel-tmux--pane-list control-buf)))
          (when panes
            (ghostel-tmux--display-pane-in-window
             (or (with-current-buffer control-buf
                   (gethash ghostel-tmux--active-pane-id ghostel-tmux--panes))
                 (car panes))
             target-window)
            (throw 'done t)))
        (accept-process-output
         (with-current-buffer control-buf
           ghostel-tmux--controller-process)
         0.1)))))

;;;###autoload
(defun ghostel-tmux-attach (session)
  "Attach to the tmux SESSION via control mode and materialize its panes."
  (interactive (list (ghostel-tmux--read-session)))
  (let ((buf (ghostel-tmux--spawn-controller
              session (list "attach-session" "-t" session))))
    (ghostel-tmux--wait-for-pane buf 5.0)))

;;;###autoload
(defun ghostel-tmux-new (session)
  "Create or attach to tmux SESSION via control mode.
Uses `tmux -CC new-session -As' which creates the session if it does
not exist, or attaches to an existing one with the same name."
  (interactive (list (read-string "tmux session name: " "main")))
  (let ((buf (ghostel-tmux--spawn-controller
              session (list "new-session" "-As" session))))
    (ghostel-tmux--wait-for-pane buf 5.0)))

;;;###autoload
(defun ghostel-tmux-detach ()
  "Detach the current tmux session.
For sessions started via `ghostel-tmux-attach' or `ghostel-tmux-new',
kills the dedicated control-mode subprocess.  For sessions taken over
from an existing ghostel shell (takeover), sends `detach-client'
and lets tmux exit control mode cleanly so the underlying shell
continues running.  Pane buffers remain visible as read-only snapshots."
  (interactive)
  (let ((controller (or ghostel-tmux--controller-buffer
                        (and (derived-mode-p 'ghostel-tmux-control-mode)
                             (current-buffer))
                        (and ghostel-tmux--dcs-active
                             (current-buffer)))))
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

(defun ghostel-tmux--cycle-pane (direction)
  "Ask tmux to make the next pane in DIRECTION (+1 or -1) active.
The visible switch follows tmux's reports."
  (let* ((controller ghostel-tmux--controller-buffer)
         (panes (and (buffer-live-p controller)
                     (ghostel-tmux--pane-list controller)))
         (here (current-buffer))
         (idx (cl-position here panes :test #'eq))
         (target (cond
                  ((null panes)
                   (user-error "Ghostel-tmux: No sibling panes"))
                  ((null idx) (car panes))
                  (t (nth (mod (+ idx direction) (length panes)) panes)))))
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
The visible switch follows tmux's reports."
  (interactive)
  (let* ((controller ghostel-tmux--controller-buffer)
         (panes (and (buffer-live-p controller)
                     (ghostel-tmux--pane-list controller)))
         (choices (mapcar (lambda (b) (cons (buffer-name b) b)) panes)))
    (unless panes
      (user-error "Ghostel-tmux: No sibling panes"))
    (when-let* ((pane (cdr (assoc (completing-read "Pane: " choices nil t)
                                  choices))))
      (ghostel-tmux--select-pane-buffer pane))))


;; One-shot snapshot of a single pane (no live connection)

;;;###autoload
(defun ghostel-tmux-capture (target)
  "Capture TARGET pane (e.g. \"session:window.pane\") into a fresh ghostel buffer.
The buffer renders the captured bytes through a VT engine but is not
connected to a live tmux session."
  (interactive
   (list (read-string "tmux target (e.g. mysession:0.0 or %3): "
                      (or (and (boundp 'ghostel-tmux--pane-id)
                               ghostel-tmux--pane-id
                               (format "%%%d" ghostel-tmux--pane-id))
                          ""))))
  (let* ((status nil)
         (output
          (with-output-to-string
            (with-current-buffer standard-output
              (setq status
                    (process-file ghostel-tmux-program nil
                                  (list t nil) nil
                                  "capture-pane" "-peJ"
                                  "-S" (number-to-string
                                        (- ghostel-tmux-history-lines))
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
