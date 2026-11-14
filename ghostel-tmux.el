;;; ghostel-tmux.el --- Tmux control mode for ghostel -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Daniel Kraus <daniel@kraus.my>

;; Author: Daniel Kraus <daniel@kraus.my>
;; URL: https://github.com/dakra/ghostel-tmux
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1") (ghostel "0.65.0"))
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
;; `ghostel-control-mode-function'.  Each pane is a terminal of
;; `ghostel-make-terminal' that gets the pane's output through
;; `ghostel-write-output', and outbound keystrokes are routed through
;; its SEND function to the control-mode channel as `send-keys -H'
;; commands.

;;; Code:

(require 'ghostel)
(require 'cl-lib)
(require 'subr-x)
(require 'tab-line)


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

(defcustom ghostel-tmux-socket nil
  "The socket of the tmux server to use, or nil for the default server.
A name is passed as `-L NAME', an absolute path as `-S PATH'.  With a
remote `default-directory', the path is on that host.  The value for a
host can be set with connection-local variables."
  :type '(choice (const :tag "Default server" nil)
                 (string :tag "Socket name or path"))
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

(defcustom ghostel-tmux-split-windows t
  "Non-nil to show the panes of a tmux window at once, as tmux lays them out.
The Emacs window showing a pane is split to match the layout of the
pane's tmux window, and tmux lays the panes out in that window's size;
the tab line has a tab per tmux window.  With nil, an Emacs window shows
one pane, which tmux zooms in a window of several, and the tab line has
a tab per pane.  Set with Customize or `customize-set-variable', a new
value applies to the windows shown already."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'ghostel-tmux--split-windows-changed)
           (ghostel-tmux--split-windows-changed)))
  :group 'ghostel-tmux)

(defcustom ghostel-tmux-keep-detached-panes 'ended
  "Whether the pane buffers of a session that left Emacs stay, read-only.
With `ended', they stay when the session ended, as after its last shell
exited or tmux was killed, and go after a detach, as the session goes on in
tmux and attaching again shows its panes.  A session whose end is not known
counts as ended: one not detached from Emacs that was a takeover, on another
host, or whose client was killed in Emacs.  t keeps them always, nil never.
A split of their windows collapses to one window.  Attaching to their
session again makes them live panes."
  :type '(choice (const :tag "When the session ended" ended)
                 (const :tag "Always" t)
                 (const :tag "Never" nil))
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
;; pane buffers: a hidden buffer owning a `tmux -C' subprocess, or the host
;; buffer of a takeover.

(defvar-local ghostel-tmux--controller-process nil
  "The `tmux -C' subprocess hosted in this controller buffer.")

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
window), :zooming (see `ghostel-tmux--ensure-pane-zoomed'), :zoomed (as
`%layout-change' last said), :tree (the visible layout, see
`ghostel-tmux--parse-layout-tree'), :settling (see
`ghostel-tmux--settle-layout'), :tile-tree and :tiled, the layout and
the panes its tile was built with, :tile-size, its pixel size when last
fitted or checked, :resize-pending (see `ghostel-tmux--tile-resize'),
and :swap-order and :swap-query (see `ghostel-tmux--tile-swap').")

(defvar-local ghostel-tmux--detaching nil
  "Non-nil once `ghostel-tmux-detach' detached the session.
The value `keep' keeps its pane buffers, see `ghostel-tmux--keep-panes-p'.")

(defvar-local ghostel-tmux--client-size nil
  "The size last sent with `refresh-client -C', as (COLS . ROWS).")

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

(defvar-local ghostel-tmux--auto-host-window nil
  "When non-nil, the window to show tmux's active pane in once it is known.
Set when a takeover starts, for the pane to replace the host's view, when
a dedicated controller starts, for its selected window, on a switch to another
session, whose panes replace the ones shown, when the active pane or the one
standing in for it goes away, and for a tmux window asked for by its tab.")

(defvar-local ghostel-tmux--auto-host-buffer nil
  "The buffer the window of a dedicated controller's attach showed then.
Until tmux first lists the session's windows; that window is left alone
once it shows another buffer.")

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

(defvar-local ghostel-tmux--server-key nil
  "The tmux server's host, pid and start time, or nil before known.
Set in a controller and in its pane buffers: pane ids are unique on a
server, so a sealed pane of the same key and id is that pane again.")

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

(defvar ghostel-tmux--decode-buffer nil
  "Unibyte work buffer of `ghostel-tmux--decode-output'.")

(defun ghostel-tmux--decode-output (data)
  "Return DATA with tmux wire escapes decoded to raw bytes.
The escapes are 3-digit octal `\\NNN' and `\\\\' (capture-pane -C).  The
result is a unibyte string suitable for `ghostel-write-output'."
  (let ((data (if (multibyte-string-p data)
                  (encode-coding-string data 'binary)
                data)))
    (if (not (string-search "\\" data))
        data
      (with-current-buffer
          (if (buffer-live-p ghostel-tmux--decode-buffer)
              ghostel-tmux--decode-buffer
            (setq ghostel-tmux--decode-buffer
                  (with-current-buffer
                      (get-buffer-create " *ghostel-tmux-decode*" t)
                    (set-buffer-multibyte nil)
                    (current-buffer))))
        (let ((inhibit-modification-hooks t))
          (erase-buffer)
          (insert data)
          (goto-char (point-min))
          (while (search-forward "\\" nil t)
            (let ((a (char-after))
                  (b (char-after (1+ (point))))
                  (c (char-after (+ (point) 2))))
              (cond
               ((eq a ?\\) (delete-char 1))
               ((and a b c (<= ?0 a ?7) (<= ?0 b ?7) (<= ?0 c ?7))
                (delete-region (1- (point)) (+ (point) 3))
                (insert (+ (* 64 (- a ?0)) (* 8 (- b ?0)) (- c ?0))))))))
        (buffer-string)))))


;; Layout string parser

;; Layout grammar (ghostty/src/terminal/tmux/layout.zig):
;;
;;   layout    := CHECKSUM ',' node
;;   node      := WIDTHxHEIGHT,X,Y(content)
;;   content   := ',' PANE_ID                      ; leaf
;;              | '{' node (',' node)* '}'         ; side by side
;;              | '[' node (',' node)* ']'         ; stacked
;;
;; A node is (TYPE WIDTH HEIGHT X Y PAYLOAD): TYPE `pane' with the pane
;; id as PAYLOAD, or `h' (side by side) or `v' (stacked) with the list of
;; child nodes.

(defun ghostel-tmux--parse-layout-tree (layout)
  "Parse a tmux LAYOUT string into its root node, or nil when it is empty.
The leading 4-char checksum and its comma are ignored."
  (when (and layout (> (length layout) 5))
    (let ((idx 5))                        ; skip "XXXX,"
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
                 (list 'pane w h x y (read-int)))
                ((or (eq next ?\{) (eq next ?\[))
                 (let ((closer (if (eq next ?\{) ?\} ?\]))
                       (children nil))
                   (cl-incf idx)
                   (push (parse-node) children)
                   (while (and (< idx (length layout))
                               (eq (aref layout idx) ?,))
                     (cl-incf idx)
                     (push (parse-node) children))
                   (expect closer)
                   (list (if (eq next ?\{) 'h 'v) w h x y
                         (nreverse children))))
                (t
                 (error "Tmux layout: unexpected %s at %d in %s"
                        (and next (string next)) idx layout))))))
        (parse-node)))))

(defun ghostel-tmux--layout-leaves (node)
  "Return the pane records of layout NODE, each (PANE-ID WIDTH HEIGHT X Y)."
  (if (eq (car node) 'pane)
      (list (list (nth 5 node) (nth 1 node) (nth 2 node)
                  (nth 3 node) (nth 4 node)))
    (mapcan #'ghostel-tmux--layout-leaves (nth 5 node))))

(defun ghostel-tmux--parse-layout (layout)
  "Parse a tmux LAYOUT string and return a list of pane records.
Each record is (PANE-ID WIDTH HEIGHT X Y)."
  (when-let* ((tree (ghostel-tmux--parse-layout-tree layout)))
    (ghostel-tmux--layout-leaves tree)))


;; Wire-format parser

;; Control mode emits a stream framed line-by-line.  States:
;;
;;   preamble     -- before the DCS `\eP1000p' of `tmux -CC'.  Drop bytes
;;                   until we see it, or until the first `%' (`tmux -C'
;;                   sends no DCS).
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
    ;; `tmux -C' sends no DCS: a `%' at line-start switches to idle.
    ;; Anything before that `%' is dispatched as idle lines, where
    ;; non-protocol lines are dropped.
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
      (ghostel-end-terminal)
      (setq ghostel-tmux--controller-buffer nil)
      (when (and (stringp mode-name) (not (string-suffix-p ":detached" mode-name)))
        (setq mode-name (concat mode-name ":detached")))
      (ghostel-tmux--drop-tab-line))))

(defun ghostel-tmux--keep-panes-p (alive)
  "Return non-nil when the panes of a session that left stay as snapshots.
ALIVE is non-nil when the session goes on in tmux, `keep' when it was detached
with KEEP, see `ghostel-tmux-detach'.  See `ghostel-tmux-keep-detached-panes'."
  (cond
   ((eq alive 'keep))
   ((eq ghostel-tmux-keep-detached-panes 'ended) (not alive))
   (t ghostel-tmux-keep-detached-panes)))

(defun ghostel-tmux--controller-panes (controller)
  "Return the pane buffers of CONTROLLER.
CONTROLLER may already be a killed buffer; its pane table is gone then,
so panes are found by scanning `buffer-list' for pane buffers
whose back-pointer matches."
  (if (buffer-live-p controller)
      (with-current-buffer controller
        (let (panes)
          (when ghostel-tmux--panes
            (maphash (lambda (_pid buf)
                       (when (buffer-live-p buf)
                         (push buf panes)))
                     ghostel-tmux--panes))
          panes))
    (cl-remove-if-not (lambda (buf)
                        (and (buffer-local-value 'ghostel-tmux-pane-mode buf)
                             (eq (buffer-local-value 'ghostel-tmux--controller-buffer buf)
                                 controller)))
                      (buffer-list))))

(defun ghostel-tmux--seal-panes (controller &optional alive)
  "Seal every pane buffer belonging to CONTROLLER read-only, or kill it.
ALIVE is non-nil when the session goes on in tmux, see
`ghostel-tmux--keep-panes-p'.  Each of its tiles collapses to one
window first."
  (ghostel-tmux--tile-dissolve controller t)
  (let ((keep (ghostel-tmux--keep-panes-p alive)))
    (dolist (buf (ghostel-tmux--controller-panes controller))
      (ghostel-tmux--seal-pane-buffer buf)
      (unless keep
        (kill-buffer buf)))))

(defun ghostel-tmux-reset ()
  "Reset the tmux session this buffer belongs to.
Kills a dedicated `tmux -C' subprocess or exits control mode in an
auto-takeover host; pane buffers are sealed read-only or killed, see
`ghostel-tmux-keep-detached-panes'.  Escape hatch for a wedged parser."
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
        (ghostel-send-string "\n" t))
       (t
        (when (and ghostel-tmux--controller-process
                   (process-live-p ghostel-tmux--controller-process))
          (delete-process ghostel-tmux--controller-process))
        (ghostel-tmux--seal-panes controller)
        (message "Ghostel-tmux: Session reset"))))))


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
;; `ghostel-tmux-attach' / `ghostel-tmux-new') owns a `tmux -C' subprocess
;; on a pipe whose stdin accepts commands directly.  An *auto-takeover
;; host* rides an existing ghostel shell's PTY: commands must go through
;; the terminal, as `ghostel-send-string' with RAW does, because with the
;; native PTY backend the buffer's process is only the event pipe —
;; writing to it goes nowhere.
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
    ghostel-tmux--dcs-active))

(defun ghostel-tmux--controller-send (data)
  "Send DATA (raw command bytes) to tmux from the current controller buffer."
  (cond
   (ghostel-tmux--controller-process
    (when (process-live-p ghostel-tmux--controller-process)
      (process-send-string ghostel-tmux--controller-process data)))
   (ghostel-tmux--dcs-active
    (ghostel-send-string data t))))


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

(defun ghostel-tmux--find-snapshot (server-key pane-id)
  "Return a sealed buffer of pane PANE-ID on the server SERVER-KEY, or nil."
  (and server-key
       (cl-find-if
        (lambda (buf)
          (and (buffer-local-value 'ghostel-tmux-pane-mode buf)
               (null (buffer-local-value 'ghostel-tmux--controller-buffer buf))
               (eql (buffer-local-value 'ghostel-tmux--pane-id buf) pane-id)
               (equal (buffer-local-value 'ghostel-tmux--server-key buf)
                      server-key)))
        (buffer-list))))

(defun ghostel-tmux--get-or-create-pane (controller pane-id window-id
                                                    &optional window-name)
  "Return the pane buffer for PANE-ID under CONTROLLER, creating if needed.
A sealed buffer of the pane becomes its buffer again."
  (with-current-buffer controller
    (let ((existing (gethash pane-id ghostel-tmux--panes)))
      (cond
       ((and existing (buffer-live-p existing))
        (with-current-buffer existing
          (setq ghostel-tmux--window-id window-id))
        existing)
       (t
        (let* ((session-name ghostel-tmux--session-name)
               (server-key ghostel-tmux--server-key)
               (name (funcall ghostel-tmux-buffer-name-function
                              (or session-name "?")
                              window-name
                              pane-id))
               (snapshot (ghostel-tmux--find-snapshot server-key pane-id))
               (buf (ghostel-make-terminal
                     (or snapshot name)
                     :size (cons ghostel-tmux-default-cols ghostel-tmux-default-rows)
                     :send (apply-partially #'ghostel-tmux--send-bytes-from-pane
                                            controller pane-id)
                     :resize #'ghostel-tmux--resize-pane)))
          (puthash pane-id buf ghostel-tmux--panes)
          (when snapshot
            (with-current-buffer snapshot
              (rename-buffer name t)
              (setq ghostel-tmux--initialized nil
                    ghostel-tmux--bootstrap-pending nil)
              (when (stringp mode-name)
                (setq mode-name (string-remove-suffix ":detached" mode-name)))))
          (with-current-buffer buf
            ;; A local directory that is gone, such as the one an attach
            ;; started in, would fail every command run from the pane.
            (unless (or (file-remote-p default-directory)
                        (file-directory-p default-directory))
              (setq default-directory (expand-file-name "~/")))
            (ghostel-tmux-pane-mode 1)
            (setq ghostel-tmux--controller-buffer controller
                  ghostel-tmux--pane-id pane-id
                  ghostel-tmux--window-id window-id
                  ghostel-tmux--server-key server-key))
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
          (ghostel-tmux--hand-over-tile-windows buf)
          (kill-buffer buf))))))

(defun ghostel-tmux--hand-over-tile-windows (buf)
  "Show another pane of its tmux window in each tile window showing BUF.
BUF is about to be killed: a buffer Emacs picks for those windows, such
as a pane of another tmux window, would take the tile's place.  The
tile is built again from tmux's next layout."
  (let* ((wid (buffer-local-value 'ghostel-tmux--window-id buf))
         (other (cl-some (lambda (pid)
                           (let ((b (gethash pid ghostel-tmux--panes)))
                             (and (buffer-live-p b) (not (eq b buf)) b)))
                         (plist-get (gethash wid ghostel-tmux--windows) :panes))))
    (when other
      (dolist (window (get-buffer-window-list buf nil t))
        (when (ghostel-tmux--tile-leaf window (current-buffer) wid)
          (set-window-buffer window other t))))))

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
            ;; tmux answers the program's queries; a reply sent with
            ;; `send-keys' would arrive after the program asking has
            ;; usually exited, in the next prompt as if typed.
            (ghostel-write-output decoded t)))))))

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

(defconst ghostel-tmux--window-rows-command
  "list-windows -F \"#{window_id}\\t#{pane_id}\\t#{window_active}\\t#{window_name}\\t#{window_layout}\\t#{window_visible_layout}\""
  "The `list-windows' command whose rows `--receive-window-rows' parses.")

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
      ;; The server key, which makes sealed panes live again, and the
      ;; rows in one write: one round trip.
      (let ((command (concat "display-message -p '#{host} #{pid} #{start_time}'\n"
                             ghostel-tmux--window-rows-command))
            (key-done nil))
        (letrec ((on-block
                  (lambda (output)
                    (cond
                     ((and key-done output)
                      (ghostel-tmux--receive-window-rows output t))
                     (key-done
                      ;; Without rows, nothing is to fill the window later.
                      (setq ghostel-tmux--auto-host-window nil
                            ghostel-tmux--auto-host-buffer nil)
                      (message "Ghostel-tmux: tmux did not list the windows of %s"
                               ghostel-tmux--session-name))
                     (t
                      (let ((key (and output (string-trim output))))
                        (setq key-done t
                              ghostel-tmux--server-key
                              (and key (not (string-empty-p key)) key)
                              ghostel-tmux--command-in-flight
                              (cons command on-block))))))))
          (ghostel-tmux--queue-command command on-block))))
     (changed
      ;; Switching sessions resets pane state.  Wipe and reload; the
      ;; Emacs window goes on showing the new session.
      (ghostel-tmux--tile-dissolve (current-buffer) t)
      (setq ghostel-tmux--auto-host-window
            (or (ghostel-tmux--pane-window) ghostel-tmux--auto-host-window))
      (ghostel-tmux--wipe-panes)
      (ghostel-tmux--request-window-rows t)))))

(defun ghostel-tmux--request-window-rows (&optional seed)
  "Ask tmux for the session's windows, which creates any pane missing.
SEED is passed on to `ghostel-tmux--receive-window-rows'."
  (ghostel-tmux--queue-command
   ghostel-tmux--window-rows-command
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
tmux also sends this when another session, such as a grouped one, lets go of a
window this session keeps, so ask.  A session that ends sends `%exit' instead."
  (when (gethash wid ghostel-tmux--windows)
    (ghostel-tmux--queue-command
     "list-windows -F \"#{window_id}\""
     (lambda (output)
       (when (and output
                  (not (member (format "@%d" wid) (split-string output))))
         (ghostel-tmux--drop-window wid))))))

(defun ghostel-tmux--drop-window (wid)
  "Forget window WID and kill its pane buffers.  Its tile collapses."
  (when-let* ((windows (ghostel-tmux--tile-windows (current-buffer) wid)))
    (unless (ghostel-tmux--hold #'ghostel-tmux--tile-update wid)
      (ghostel-tmux--tile-collapse windows)))
  (let ((win (gethash wid ghostel-tmux--windows)))
    (when win
      (let ((panes (plist-get win :panes)))
        (dolist (pid panes)
          (ghostel-tmux--remove-pane (current-buffer) pid wid))))
    (remhash wid ghostel-tmux--windows))
  (setq ghostel-tmux--window-order
        (delq wid ghostel-tmux--window-order))
  (ghostel-tmux--refresh-tab-line))

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

(defvar ghostel-tmux--held-calls nil
  "Calls waiting for the minibuffer to close, the newest first.
Each is (CONTROLLER FUNCTION . ARGS), see `ghostel-tmux--hold'.")

(defun ghostel-tmux--hold (function &rest args)
  "Keep the call of FUNCTION with ARGS for later while a minibuffer is open.
Return non-nil when it is kept.  Emacs shows the windows as they were
before a minibuffer again when it closes, which would undo what tmux
asked for meanwhile.  The call is made in this controller once no minibuffer
is open, after the calls kept before it; one kept twice is made once."
  (when (active-minibuffer-window)
    (let ((call `(,(current-buffer) ,function ,@args)))
      (setq ghostel-tmux--held-calls
            (cons call (delete call ghostel-tmux--held-calls))))
    (add-hook 'minibuffer-exit-hook #'ghostel-tmux--schedule-held-calls)
    t))

(defun ghostel-tmux--schedule-held-calls ()
  "Run `ghostel-tmux--run-held-calls' once Emacs has the windows back."
  (run-at-time 0 nil #'ghostel-tmux--run-held-calls))

(defun ghostel-tmux--run-held-calls ()
  "Make the calls `ghostel-tmux--hold' kept, unless a minibuffer is open.
A call that signals an error is reported and the others are made."
  (unless (active-minibuffer-window)
    (remove-hook 'minibuffer-exit-hook #'ghostel-tmux--schedule-held-calls)
    (pcase-dolist (`(,controller ,function . ,args)
                   (nreverse (prog1 ghostel-tmux--held-calls
                               (setq ghostel-tmux--held-calls nil))))
      (when (buffer-live-p controller)
        (with-current-buffer controller
          (condition-case err
              (apply function args)
            (error
             (message "Ghostel-tmux: Error in %s: %s"
                      function (error-message-string err)))))))))

(defun ghostel-tmux--follow-held (switch)
  "Follow the pane tmux has active now, for a report held back.
SWITCH is as for `ghostel-tmux--follow-active-pane'."
  (ghostel-tmux--follow-active-pane ghostel-tmux--active-pane-id switch))

(defun ghostel-tmux--follow-active-pane (new-pid &optional switch)
  "Redirect an Emacs window showing a sibling pane to NEW-PID.
Runs for every switch tmux reports: `%window-pane-changed',
`%session-window-changed', or a deferred re-trigger from
`--on-layout-change' when a report raced ahead of the pane buffer's
creation.  A switch made in Emacs reaches tmux through
`--select-pane', and its report then finds the pane shown, unless the
window is zoomed in a tile; the report of a switch that a newer one
replaces is not followed, see `ghostel-tmux--selection-pending-p'.
A pane some window already shows leaves the other windows alone.
`ghostel-tmux--auto-host-window', the window of a pane that is gone,
comes before any other, then the selected window on a SWITCH to
another tmux window, then a window showing a pane of the same tmux
window.  In a tile, the window of NEW-PID is selected instead, when
the selected window is one of the tile's, and the tile's size reported.
Outside a tile, the window found waits for the minibuffer to close as
`ghostel-tmux--auto-host-window', and then gets the pane tmux has
active, see `ghostel-tmux--hold'."
  (let* ((new-buf (and new-pid ghostel-tmux--panes
                       (gethash new-pid ghostel-tmux--panes)))
         (tile (and ghostel-tmux-split-windows
                    (buffer-live-p new-buf)
                    (ghostel-tmux--tile-windows
                     (current-buffer)
                     (buffer-local-value 'ghostel-tmux--window-id new-buf)))))
    (cond
     ((not (buffer-live-p new-buf)))
     (tile
      (when (memq ghostel-tmux--auto-host-window tile)
        (setq ghostel-tmux--auto-host-window nil))
      (ghostel-tmux--select-tile-window tile new-pid)
      (ghostel-tmux--tile-sync-size
       (buffer-local-value 'ghostel-tmux--window-id new-buf)))
     (t
      (let ((wid (buffer-local-value 'ghostel-tmux--window-id new-buf))
            (selected (or (minibuffer-selected-window) (selected-window)))
            pane-bufs target-window sibling-window)
        (maphash (lambda (_pid buf) (push buf pane-bufs))
                 ghostel-tmux--panes)
        (walk-windows
         (lambda (window)
           (let ((buf (window-buffer window)))
             (when (and (memq buf pane-bufs) (not (eq buf new-buf)))
               (unless target-window
                 (setq target-window window))
               (when (and (not sibling-window)
                          (eql (buffer-local-value 'ghostel-tmux--window-id buf) wid))
                 (setq sibling-window window)))))
         'no-minibuffer 'visible)
        (when sibling-window
          (setq target-window sibling-window))
        (when (and switch
                   (memq (window-buffer selected) pane-bufs)
                   (not (eq (window-buffer selected) new-buf)))
          (setq target-window selected))
        (when (window-live-p ghostel-tmux--auto-host-window)
          (setq target-window ghostel-tmux--auto-host-window))
        (setq ghostel-tmux--auto-host-window nil)
        (when (and target-window
                   (not (get-buffer-window new-buf 'visible)))
          (if (ghostel-tmux--hold #'ghostel-tmux--follow-held switch)
              (setq ghostel-tmux--auto-host-window target-window)
            (ghostel-tmux--display-pane-in-window new-buf target-window))))))))

(defun ghostel-tmux--select-tile-window (windows pid)
  "Select the window of pane PID among the tile WINDOWS, which shows it.
Only when the selected window is one of them: a report from tmux must
not take the selection from another window or the minibuffer.  With the
minibuffer selected, the pane tmux has active then is followed when it
has closed, see `ghostel-tmux--hold'.  Runs in the controller."
  (when-let* ((buf (gethash pid ghostel-tmux--panes))
              (window (cl-find-if (lambda (w)
                                    (and (eql (ghostel-tmux--tile-pid w) pid)
                                         (eq (window-buffer w) buf)))
                                  windows)))
    (cond
     ((memq (selected-window) windows)
      (unless (eq window (selected-window))
        (save-current-buffer
          (select-window window))))
     ((window-minibuffer-p (selected-window))
      (ghostel-tmux--hold #'ghostel-tmux--follow-held nil)))))

(defun ghostel-tmux--selection-pending-p (pid)
  "Non-nil when a `select-pane' of a pane other than PID awaits its reply.
Its report follows, so a report of PID is from a selection it replaces."
  (cl-some (lambda (entry)
             (and entry
                  (string-match "\\`select-pane -t %\\([0-9]+\\)\\'" (car entry))
                  (not (eql pid (string-to-number (match-string 1 (car entry)))))))
           (cons ghostel-tmux--command-in-flight ghostel-tmux--command-queue)))

(defun ghostel-tmux--on-window-pane-changed (wid pid)
  "Handle `%window-pane-changed @WID %PID': record and follow.
Fires when tmux's active pane changes *within* a window (e.g. `select-pane',
kill-pane auto-survivor pick), in any window of the server.
Each window's active pane is remembered so `--on-session-window-changed' can
restore it on cycle-window; only a change in the session's current window moves
the view.  Switching between windows is signalled by `%session-window-changed'
and handled separately."
  (let ((entry (gethash wid ghostel-tmux--windows)))
    (when entry
      (puthash wid (plist-put entry :active-pane pid)
               ghostel-tmux--windows)))
  (when (and (eql wid ghostel-tmux--current-window-id)
             (not (ghostel-tmux--selection-pending-p pid)))
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
one whose recorded pane still has its buffer elsewhere.
A switch that a selection on its way replaces is ignored."
  (when (and (eql sid ghostel-tmux--session-id)
             (not (ghostel-tmux--selection-pending-p
                   (plist-get (gethash wid ghostel-tmux--windows) :active-pane))))
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
        (ghostel-tmux--follow-active-pane target t)
        (when (and recorded (not known))
          (setq ghostel-tmux--pending-follow-wid wid)))
       (t
        (setq ghostel-tmux--pending-follow-wid wid))))))

(defun ghostel-tmux--on-layout-change (wid layout &optional visible-layout flags)
  "Handle `%layout-change @WID LAYOUT VISIBLE-LAYOUT FLAGS' — reconcile panes.
LAYOUT has every pane of the window, at its unzoomed size in a zoomed
window; VISIBLE-LAYOUT has the panes on screen at their real size.
FLAGS are the window's flags, nil when not known."
  (let* ((tree (ghostel-tmux--parse-layout-tree layout))
         (panes (ghostel-tmux--layout-leaves tree))
         (visible-tree (and visible-layout
                            (ghostel-tmux--parse-layout-tree visible-layout)))
         (visible (ghostel-tmux--layout-leaves visible-tree))
         (pane-ids (mapcar #'car panes))
         (existing (gethash wid ghostel-tmux--windows))
         (old-ids (and existing (plist-get existing :panes)))
         (controller (current-buffer))
         (window-name (and existing (plist-get existing :name))))
    (let ((entry (or existing (list :name window-name))))
      (setq entry (plist-put entry :panes pane-ids))
      (setq entry (plist-put entry :zoomed
                             (if flags
                                 (and (string-search "Z" flags) t)
                               (< (length visible) (length panes)))))
      ;; A tile takes its layout from `--settle-layout' only.
      (unless (ghostel-tmux--tile-windows controller wid)
        (setq entry (plist-put entry :tree (or visible-tree tree))))
      (puthash wid entry ghostel-tmux--windows))
    (unless (memq wid ghostel-tmux--window-order)
      (setq ghostel-tmux--window-order
            (append ghostel-tmux--window-order (list wid))))
    (dolist (pid old-ids)
      (unless (memq pid pane-ids)
        (ghostel-tmux--remove-pane controller pid wid)))
    (dolist (rec panes)
      (let* ((pid (nth 0 rec))
             (leaf (or (assq pid visible) rec))
             (size (cons (nth 1 leaf) (nth 2 leaf)))
             (buf (ghostel-tmux--get-or-create-pane
                   controller pid wid window-name)))
        (with-current-buffer buf
          (let ((new (not (or ghostel-tmux--initialized
                              ghostel-tmux--bootstrap-pending))))
            ;; A pane that no window shows takes tmux's size, and so does
            ;; a new one, which can be a sealed buffer shown already.  One
            ;; shown outside a tile has the size of its Emacs window, which
            ;; `--resize-pane' reports to tmux; one in a tile gets it from
            ;; `--settle-layout'.
            (when (or new (not (get-buffer-window-list buf nil 'visible)))
              (ghostel-tmux--set-pane-size buf size))
            (when new
              (setq ghostel-tmux--bootstrap-pending t)
              (ghostel-tmux--bootstrap-pane controller pid))))))
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
          (setq ghostel-tmux--active-pane-id shown)
          (unless (ghostel-tmux--hold #'ghostel-tmux--follow-held nil)
            (let ((pane (gethash shown ghostel-tmux--panes))
                  (target-window ghostel-tmux--auto-host-window))
              (setq ghostel-tmux--auto-host-window nil)
              (when (buffer-live-p pane)
                (ghostel-tmux--display-pane-in-window pane target-window)))))
        ;; Deferred follow: `%session-window-changed' fired for this WID
        ;; before its panes were there (typical for new-window — the
        ;; session-window-changed arrives before window-add + list-windows
        ;; materialize the layout), or while its active pane is not among
        ;; them.
        (when (eql ghostel-tmux--pending-follow-wid wid)
          (setq ghostel-tmux--pending-follow-wid nil
                ghostel-tmux--active-pane-id shown)
          (ghostel-tmux--follow-active-pane shown t))))
    ;; Also catch the within-window race: `%window-pane-changed'
    ;; arrived before `%layout-change' created the pane buffer.
    (when (and ghostel-tmux--active-pane-id
               (memq ghostel-tmux--active-pane-id pane-ids))
      (ghostel-tmux--follow-active-pane ghostel-tmux--active-pane-id))
    (when (and ghostel-tmux-split-windows
               (or (ghostel-tmux--tile-windows controller wid)
                   (ghostel-tmux--pane-window-of wid)))
      (ghostel-tmux--settle-layout wid))
    (ghostel-tmux--rezoom-shown-pane wid flags)
    (ghostel-tmux--refresh-tab-line)))

(defun ghostel-tmux--rezoom-shown-pane (wid flags)
  "Zoom the pane shown of window WID again, unless FLAGS say it is zoomed.
Only without `ghostel-tmux-split-windows'.  tmux unzooms a window when a pane
joins or leaves it, while the pane shown keeps the size of its Emacs window."
  (let* ((pid ghostel-tmux--active-pane-id)
         (buf (and pid (gethash pid ghostel-tmux--panes))))
    (when (and (not ghostel-tmux-split-windows)
               (eql wid ghostel-tmux--current-window-id)
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
the session's current window, before any pane is created, and its active pane,
once all are: a sealed pane made live again can be shown already, and its window
is not to follow the active pane.  The window of
`ghostel-tmux--auto-host-window' keeps such a pane, and tmux switches to it,
or shows the active one; the window of an attach does so only while it shows
the buffer it showed then.  Later changes arrive as notifications."
  (let ((rows (delq nil (mapcar #'ghostel-tmux--parse-window-row
                                (split-string output "\n" t))))
        (kept (and seed ghostel-tmux--auto-host-window))
        active-pane)
    (when (and seed (null ghostel-tmux--current-window-id))
      (when-let* ((row (cl-find-if #'caddr rows)))
        (setq ghostel-tmux--current-window-id (car row)
              active-pane (cadr row))))
    (when kept
      (setq ghostel-tmux--auto-host-window nil))
    (pcase-dolist (`(,wid ,active ,_current ,name ,layout ,visible) rows)
      (let ((entry (plist-put (gethash wid ghostel-tmux--windows)
                              :name name)))
        (puthash wid (plist-put entry :active-pane active)
                 ghostel-tmux--windows))
      (ghostel-tmux--on-layout-change wid layout visible))
    (when (and active-pane (null ghostel-tmux--active-pane-id))
      (setq ghostel-tmux--active-pane-id active-pane))
    (when seed
      (when ghostel-tmux--auto-host-buffer
        (message "Ghostel-tmux: Attached to %s" ghostel-tmux--session-name))
      (ghostel-tmux--fill-kept-window kept))
    ;; All of the session's windows, in tmux's order; a window linked
    ;; twice has two rows.
    (when rows
      (setq ghostel-tmux--window-order (delete-dups (mapcar #'car rows))))
    (when (assq ghostel-tmux--current-window-id rows)
      (ghostel-tmux--leave-floating-window))))

(defun ghostel-tmux--fill-kept-window (kept)
  "Show a pane in KEPT, the window kept for the rows of the session.
The window of an attach must still show `ghostel-tmux--auto-host-buffer'.
A pane of this controller that KEPT shows stays, and tmux switches to
it; any other buffer makes way for tmux's active pane.  Without a pane
to show, KEPT is `ghostel-tmux--auto-host-window' again.  Waits for the
minibuffer to close, see `ghostel-tmux--hold'."
  (unless (and (or kept ghostel-tmux--auto-host-buffer)
               (ghostel-tmux--hold #'ghostel-tmux--fill-kept-window kept))
    (when (and ghostel-tmux--auto-host-buffer
               (not (and (window-live-p kept)
                         (eq (window-buffer kept) ghostel-tmux--auto-host-buffer))))
      (setq kept nil))
    (setq ghostel-tmux--auto-host-buffer nil)
    (when (window-live-p kept)
      (let* ((shown (window-buffer kept))
             (pane (if (eq (buffer-local-value 'ghostel-tmux--controller-buffer
                                               shown)
                           (current-buffer))
                       shown
                     (ghostel-tmux--attach-pane))))
        (if pane
            (ghostel-tmux--display-pane-in-window pane kept)
          (setq ghostel-tmux--auto-host-window kept))))))

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
buffer there is the user's.  Waits for the minibuffer to close, as the
window is kept after that, see `ghostel-tmux--fill-kept-window'."
  (when (and (not (ghostel-tmux--hold #'ghostel-tmux--leave-floating-window))
             (window-live-p ghostel-tmux--auto-host-window)
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
    pane_height client_pid pane_pid)
  "The tmux formats that describe a pane beyond its text.")

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
blank rows below the cursor first, as tmux does when it shrinks the pane.
A capture ends without resetting its colors."
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
for, each nil for a failed command.  The tty of a dedicated controller's
pane on this machine is read for password prompts."
  (let ((buf (with-current-buffer controller
               (gethash pane-id ghostel-tmux--panes)))
        (state (ghostel-tmux--parse-pane-state state)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; A pane killed while its backfill was queued gets a second one.
        (when (and rows (not ghostel-tmux--initialized))
          (ghostel-write-output
           (ghostel-tmux--backfill-data
            state primary rows screen (cdr (ghostel-terminal-size)))
           t))
        (setq ghostel-tmux--initialized t
              ghostel-tmux--bootstrap-pending nil)
        (ghostel-set-password-pid
         (ghostel-tmux--local-pane-pid controller state))))))

(defun ghostel-tmux--local-pane-pid (controller state)
  "Return the pid in STATE of a pane of CONTROLLER on this machine, or nil.
STATE is from `ghostel-tmux--parse-pane-state'.  The pane is on this machine
when tmux's client is CONTROLLER's local subprocess.  A takeover has none:
once its `tmux -CC' exits, what it sends reaches the shell it ran in."
  (with-current-buffer controller
    (when-let* ((proc ghostel-tmux--controller-process)
                ((not (file-remote-p default-directory)))
                ((equal (number-to-string (process-id proc))
                        (alist-get 'client_pid state)))
                (pid (string-to-number (alist-get 'pane_pid state)))
                ((> pid 0)))
      pid)))

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
under the typical 4 KB limit of a PTY's line discipline.")

(defun ghostel-tmux--send-bytes-from-pane (controller pane-id data)
  "Send DATA bytes to tmux PANE-ID via CONTROLLER as `send-keys -H'.
Long DATA is split into several commands, as a line longer than a
PTY's input buffer is truncated."
  (when (and data (> (length data) 0)
             (buffer-live-p controller))
    (let ((unibytes (if (multibyte-string-p data)
                        (encode-coding-string data 'utf-8)
                      data)))
      (with-current-buffer controller
        (when (ghostel-tmux--controller-live-p)
          (let* ((i 0)
                 (n (length unibytes))
                 (chunk ghostel-tmux--send-keys-chunk-bytes)
                 (prefix (format "send-keys -t %%%d -H " pane-id))
                 (tail (car (last ghostel-tmux--command-queue))))
            ;; Keys typed while a command is in flight join the pane's
            ;; queued `send-keys': one round trip per burst, not per key.
            (if (and tail
                     (null (cdr tail))
                     (string-prefix-p prefix (car tail))
                     ;; A lone ESC and the next key would read as Meta.
                     (not (string-suffix-p " 1b" (car tail)))
                     (<= (+ (length (car tail)) (* 3 n))
                         (+ (length prefix) (* 3 chunk))))
                (setcar tail (concat (car tail) " "
                                     (ghostel-tmux--bytes-to-hex unibytes)))
              (while (< i n)
                (let* ((end (min n (+ i chunk)))
                       (hex (ghostel-tmux--bytes-to-hex
                             (substring unibytes i end))))
                  (ghostel-tmux--queue-command (concat prefix hex))
                  (setq i end))))))))))


;; Process filter / sentinel

(defun ghostel-tmux--filter (proc string)
  "Process filter for the `tmux -C' subprocess PROC; STRING is fresh output."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (ghostel-tmux--feed-bytes string)))))

(defun ghostel-tmux--sentinel (proc event)
  "Sentinel for the `tmux -C' subprocess.  PROC ended with EVENT.
When the controller dies — whether cleanly or because tmux got killed
externally — each pane buffer is sealed or killed, see
`ghostel-tmux-keep-detached-panes', also when the controller buffer
itself was killed before the process died.  The controller buffer has
no further use and goes away."
  (unless (process-live-p proc)
    (let* ((buf (process-buffer proc))
           (suffix "")
           (detached (and (buffer-live-p buf)
                          (buffer-local-value 'ghostel-tmux--detaching buf)))
           ;; A local tmux that just sent `%exit' says whether the
           ;; session is still there.
           (alive (or detached
                      (and (buffer-live-p buf)
                           (with-current-buffer buf
                             (and ghostel-tmux--closing
                                  ghostel-tmux--session-name
                                  (not (file-remote-p default-directory))
                                  (ghostel-tmux--session-exists-p
                                   ghostel-tmux--session-name)))))))
      (ghostel-tmux--seal-panes buf alive)
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

(defun ghostel-tmux--prefix-zoom ()
  "Zoom the current pane, or show the panes of its window again.
Without `ghostel-tmux-split-windows', the pane shown is zoomed always."
  (interactive)
  (if ghostel-tmux-split-windows
      (ghostel-tmux--cmd (format "resize-pane -Z -t %s" (ghostel-tmux--prefix-target)))
    (message "Ghostel-tmux: Each Emacs window shows its pane zoomed")))
(define-key ghostel-tmux-prefix-map (kbd "z") #'ghostel-tmux--prefix-zoom)

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

(defun ghostel-tmux--prefix-next-pane ()
  "Switch to the next pane in the current tmux window."
  (interactive)
  (ghostel-tmux--cycle-pane 1 ghostel-tmux--window-id))
(define-key ghostel-tmux-prefix-map (kbd "o") #'ghostel-tmux--prefix-next-pane)

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
(define-key ghostel-tmux-prefix-map (kbd "r") #'ghostel-tmux-restore-split)

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
Bound as `<prefix> <prefix>', matching tmux's convention.  Only meaningful when
`ghostel-tmux-prefix-key' resolves to a single control byte (`C-<letter>')."
  (interactive)
  (let* ((seq (kbd ghostel-tmux-prefix-key))
         (byte (and (= (length seq) 1)
                    (let ((e (aref seq 0)))
                      (and (integerp e) (<= 0 e 31) e)))))
    (if byte
        (ghostel-send-string (string byte))
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
    (setq tab-line-format '(:eval (ghostel-tmux--tab-line)))
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
  "Turn off the tab line of this buffer, which has no session to show."
  (kill-local-variable 'tab-line-format)
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
  "Hidden buffer hosting the `tmux -C' subprocess."
  (buffer-disable-undo)
  (setq buffer-read-only t
        ghostel-tmux--panes (make-hash-table)
        ghostel-tmux--windows (make-hash-table))
  ;; A killed controller buffer must not leave a zombie `tmux -C'; the
  ;; sentinel then seals or kills the panes.
  (add-hook 'kill-buffer-hook #'ghostel-tmux--controller-killed nil t))

(defun ghostel-tmux--controller-killed ()
  "Tear down the `tmux -C' subprocess when its controller buffer dies."
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

(defun ghostel-tmux--set-pane-size (buf size)
  "Give the terminal of pane BUF tmux's SIZE, (COLS . ROWS), unless it has it."
  (with-current-buffer buf
    (unless (equal (ghostel-terminal-size) size)
      (ghostel-set-size size))))

(defun ghostel-tmux--resize-pane (size)
  "Resize this pane's terminal to SIZE, (COLS . ROWS), and report it to tmux.
The RESIZE function of `ghostel-make-terminal'.  An unchanged grid still
passes through, for the cell pixel size and the report: tmux's client
size may be that of another pane's window.  The pane of a tile keeps
tmux's size, with the cell size of its window, and the tile's size is
reported instead, see `ghostel-tmux--tile-check'."
  (let ((controller ghostel-tmux--controller-buffer))
    (if (and ghostel-tmux-split-windows
             (buffer-live-p controller)
             (ghostel-tmux--tile-windows controller ghostel-tmux--window-id))
        (progn
          (ghostel-set-size (ghostel-terminal-size))
          (ghostel-tmux--schedule-tile-check))
      ;; Committed now, before tmux hears of the size: committed later, the
      ;; resize would clear the prompt row the shell has repainted by then.
      (ghostel-set-size size)
      (when (buffer-live-p controller)
        (with-current-buffer controller
          (when (ghostel-tmux--controller-live-p)
            (setq ghostel-tmux--client-size size)
            (ghostel-tmux--queue-command
             (format "refresh-client -C %d,%d" (car size) (cdr size)))))))))


;; Tiles

;; With `ghostel-tmux-split-windows', the panes of a tmux window are shown
;; in a tile: Emacs windows split to match the window's visible layout,
;; each with the window parameter `ghostel-tmux-tile', (CONTROLLER WID
;; PID).  tmux owns the layout.  What the tile's windows can show is
;; reported with `refresh-client -C', and each layout tmux settles on
;; moves the window edges to its pane sizes.  Splits bind
;; `window-combination-limit', so that a tile's windows hold no other
;; window, and deleting all but one gives it the tile's space.  A window
;; of the tile that shows another buffer keeps its place.

(defvar ghostel-tmux--tiles-exist nil
  "Non-nil when some window may be part of a tile.")

(defvar ghostel-tmux--tile-timer nil
  "Timer of a pending `ghostel-tmux--tile-check'.")

(defvar ghostel-tmux--tile-members nil
  "The windows of the tiles as last checked, as (WINDOW CONTROLLER . WID).
`window-swap-states' keeps the windows and drops their parameters.")

(defun ghostel-tmux--tile-leaf (window &optional controller wid)
  "Return the tile parameter of WINDOW, or nil.
With CONTROLLER and WID, only for a window of the tile of that window.
A tile of a killed controller is none."
  (let ((tile (and (window-live-p window)
                   (window-parameter window 'ghostel-tmux-tile))))
    (and tile
         (buffer-live-p (nth 0 tile))
         (or (null controller) (eq (nth 0 tile) controller))
         (or (null wid) (eql (nth 1 tile) wid))
         tile)))

(defun ghostel-tmux--tile-pid (window)
  "Return the pane id of WINDOW's tile parameter, or nil."
  (nth 2 (window-parameter window 'ghostel-tmux-tile)))

(defun ghostel-tmux--tiles ()
  "Return the windows of all tiles, as ((CONTROLLER . WID) . WINDOWS).
The windows of each come top left first."
  (let (tiles)
    (dolist (window (ghostel-tmux--all-windows))
      (when-let* ((tile (ghostel-tmux--tile-leaf window)))
        (push window (alist-get (cons (nth 0 tile) (nth 1 tile)) tiles nil nil #'equal))))
    (mapcar (lambda (tile) (cons (car tile) (nreverse (cdr tile)))) tiles)))

(defun ghostel-tmux--leave-tile (window)
  "Make WINDOW part of no tile, with a tab line of its own."
  (setq ghostel-tmux--tile-members (assq-delete-all window ghostel-tmux--tile-members))
  (set-window-parameter window 'ghostel-tmux-tile nil)
  (set-window-parameter window 'tab-line-format nil))

(defun ghostel-tmux--recover-members ()
  "Put the windows of a tile that lost it back in their tile.
A window state put in a window of a tile, as by `window-swap-states',
drops its tile parameter; the window is back when it shows a pane of
the tile.  Then note the windows of the tiles."
  (pcase-dolist (`(,window ,controller . ,wid) ghostel-tmux--tile-members)
    (when (and (window-live-p window)
               (buffer-live-p controller)
               (not (window-parameter window 'ghostel-tmux-tile)))
      (let* ((buf (window-buffer window))
             (pid (buffer-local-value 'ghostel-tmux--pane-id buf)))
        (when (and pid
                   (eq (buffer-local-value 'ghostel-tmux--controller-buffer buf) controller)
                   (eql (buffer-local-value 'ghostel-tmux--window-id buf) wid))
          (set-window-parameter window 'ghostel-tmux-tile (list controller wid pid))))))
  (setq ghostel-tmux--tile-members
        (delq nil (mapcar (lambda (window)
                            (when-let* ((tile (ghostel-tmux--tile-leaf window)))
                              (cons window (cons (nth 0 tile) (nth 1 tile)))))
                          (ghostel-tmux--all-windows)))))

(defun ghostel-tmux--all-windows ()
  "Return the windows of all frames, each frame's top left first."
  (mapcan (lambda (frame)
            (window-list frame 'no-mini (frame-first-window frame)))
          (frame-list)))

(defun ghostel-tmux--tile-windows (controller wid)
  "Return the windows of the tile of CONTROLLER's tmux window WID."
  (when ghostel-tmux--tiles-exist
    (cl-remove-if-not (lambda (window)
                        (ghostel-tmux--tile-leaf window controller wid))
                      (ghostel-tmux--all-windows))))

(defun ghostel-tmux--window-ancestor-p (ancestor window)
  "Return non-nil when ANCESTOR is WINDOW or holds it."
  (while (and window (not (eq window ancestor)))
    (setq window (window-parent window)))
  (eq window ancestor))

(defun ghostel-tmux--window-leaves (window)
  "Return the live windows WINDOW is or holds."
  (if (window-live-p window)
      (list window)
    (let ((child (window-child window))
          (leaves nil))
      (while child
        (setq leaves (nconc leaves (ghostel-tmux--window-leaves child))
              child (window-next-sibling child)))
      leaves)))

(defun ghostel-tmux--tile-root (windows)
  "Return the smallest window holding all of WINDOWS, or nil."
  (let ((root (car windows)))
    (while (and root
                (not (cl-every (lambda (w) (ghostel-tmux--window-ancestor-p root w))
                               windows)))
      (setq root (window-parent root)))
    root))

(defun ghostel-tmux--leaf-need (window cols rows)
  "Return (WIDTH . HEIGHT), the pixel size for WINDOW to show COLS by ROWS.
That is the columns `window-max-chars-per-line' counts at WINDOW's font
width, with its continuation column, plus the pixels WINDOW has beyond
its body, and ROWS lines plus its pixels above and below its body."
  (with-selected-window window
    (let* ((font-width (window-font-width window))
           (body (window-body-width window t))
           (spare (- (/ body font-width) (window-max-chars-per-line window))))
      (cons (+ (* (+ cols spare) font-width) (- (window-pixel-width window) body))
            (+ (* rows (default-line-height))
               (- (window-pixel-height window) (window-body-height window t)))))))

(defun ghostel-tmux--least-size (window horizontal)
  "Return the pixels WINDOW takes for one column, with HORIZONTAL, or one row."
  (let ((one (ghostel-tmux--leaf-need window 1 1)))
    (if horizontal (car one) (cdr one))))

(defun ghostel-tmux--layout-need (node window-of)
  "Return (WIDTH . HEIGHT), the pixel size a tile of layout NODE takes.
WINDOW-OF is called with a pane id and returns the window to measure it
in, see `ghostel-tmux--leaf-need'."
  (if (eq (car node) 'pane)
      (ghostel-tmux--leaf-need (funcall window-of (nth 5 node))
                               (nth 1 node) (nth 2 node))
    (let ((kids (mapcar (lambda (kid) (ghostel-tmux--layout-need kid window-of))
                        (nth 5 node))))
      (if (eq (car node) 'h)
          (cons (apply #'+ (mapcar #'car kids)) (apply #'max (mapcar #'cdr kids)))
        (cons (apply #'max (mapcar #'car kids)) (apply #'+ (mapcar #'cdr kids)))))))

(defun ghostel-tmux--layout-capacity (node window-of)
  "Return (COLS . ROWS) of terminal a tile of layout NODE can show.
WINDOW-OF is called with a pane id and returns its window, or nil for
a pane that has none, which counts at tmux's size.  tmux puts a column
or a row between the parts of a split."
  (if (eq (car node) 'pane)
      (if-let* ((window (funcall window-of (nth 5 node))))
          (cons (window-max-chars-per-line window)
                (with-selected-window window (floor (window-screen-lines))))
        (cons (nth 1 node) (nth 2 node)))
    (let* ((kids (mapcar (lambda (kid) (ghostel-tmux--layout-capacity kid window-of))
                         (nth 5 node)))
           (seps (1- (length kids))))
      (if (eq (car node) 'h)
          (cons (+ (apply #'+ (mapcar #'car kids)) seps)
                (apply #'min (mapcar #'cdr kids)))
        (cons (apply #'min (mapcar #'car kids))
              (+ (apply #'+ (mapcar #'cdr kids)) seps))))))

(defun ghostel-tmux--layout-shape (node)
  "Return layout NODE without its sizes and panes."
  (if (eq (car node) 'pane)
      'pane
    (cons (car node) (mapcar #'ghostel-tmux--layout-shape (nth 5 node)))))

(defun ghostel-tmux--layout-prune (node keep)
  "Return layout NODE with only the panes KEEP returns non-nil for, or nil."
  (if (eq (car node) 'pane)
      (and (funcall keep (nth 5 node)) node)
    (let ((kids (delq nil (mapcar (lambda (kid) (ghostel-tmux--layout-prune kid keep))
                                  (nth 5 node)))))
      (if (cdr kids)
          (append (butlast node) (list kids))
        (car kids)))))

(defmacro ghostel-tmux--with-tile-resizing (&rest body)
  "Run BODY with window sizes in pixels and any small size allowed."
  (declare (indent 0) (debug t))
  `(let ((window-resize-pixelwise t)
         (window-min-width 1)
         (window-min-height 1))
     ,@body))

(defun ghostel-tmux--layout-reserve (kids window-of horizontal scale least)
  "Return the pixels to keep for the parts KIDS of a split.
Each part gets its size, see `ghostel-tmux--layout-need', in SCALE, and
at least LEAST.  HORIZONTAL is non-nil for a split side by side.
WINDOW-OF is as for `ghostel-tmux--layout-need'."
  (apply #'+ (mapcar (lambda (kid)
                       (let ((need (ghostel-tmux--layout-need kid window-of)))
                         (max least (floor (* scale (if horizontal (car need)
                                                      (cdr need)))))))
                     kids)))

(defun ghostel-tmux--tile-place (node window panes tile)
  "Show layout NODE in WINDOW, split to fit, with PANES as (PID . BUFFER).
TILE is the (CONTROLLER WID) of the window parameter.  The windows split
off WINDOW are measured as WINDOW, as they do not exist yet.  Return
the alist of (PID . WINDOW) placed: a part of a split that does not fit
leaves out the rest of that split."
  (if (eq (car node) 'pane)
      (let ((pid (nth 5 node)))
        ;; A window dedicated to another buffer stays out of the tile.
        (when (ignore-errors (set-window-buffer window (alist-get pid panes) t) t)
          (set-window-parameter window 'ghostel-tmux-tile (append tile (list pid)))
          (setf (alist-get window ghostel-tmux--tile-members) (cons (car tile) (cadr tile)))
          (list (cons pid window))))
    (let* ((horizontal (eq (car node) 'h))
           (window-of (lambda (_pid) window))
           (dimension (lambda (size) (if horizontal (car size) (cdr size))))
           (least (ghostel-tmux--least-size window horizontal))
           (need (funcall dimension (ghostel-tmux--layout-need node window-of)))
           (have (if horizontal (window-pixel-width window) (window-pixel-height window)))
           ;; A layout bigger than WINDOW, as before tmux has its size,
           ;; leaves the later parts their share.
           (scale (min 1.0 (/ (float have) (max 1 need))))
           (kids (nth 5 node))
           (win window)
           (placed nil))
      (while kids
        (let* ((rest (length (cdr kids)))
               (new (and (> rest 0)
                         (let ((size (min (funcall dimension
                                                   (ghostel-tmux--layout-need
                                                    (car kids) window-of))
                                          (- (if horizontal (window-pixel-width win)
                                               (window-pixel-height win))
                                             (ghostel-tmux--layout-reserve
                                              (cdr kids) window-of horizontal
                                              scale least)))))
                           (and (>= size least)
                                (ignore-errors
                                  (split-window win size (if horizontal 'right 'below)
                                                t)))))))
          (setq placed (nconc placed (ghostel-tmux--tile-place (car kids) win panes tile)))
          (setq kids (and new (cdr kids))
                win new)))
      placed)))

(defun ghostel-tmux--tile-fit (node placed)
  "Move the window edges of the tile showing NODE to tmux's pane sizes.
PLACED is the alist of (PID . WINDOW) of the tile.  Each part of a split
but the last is given its size, which its next sibling makes up for.  A
split bigger than its windows leaves the later parts their share."
  (let ((root (ghostel-tmux--tile-root (mapcar #'cdr placed))))
    (cl-labels
        ((window-of (pid) (or (alist-get pid placed) (cdar placed)))
         (region-of (node)
           (ghostel-tmux--tile-root
            (delq nil (mapcar (lambda (leaf) (alist-get (car leaf) placed))
                              (ghostel-tmux--layout-leaves node)))))
         (fit (node)
           (unless (eq (car node) 'pane)
             (let* ((horizontal (eq (car node) 'h))
                    (size (if horizontal #'window-pixel-width #'window-pixel-height))
                    (dimension (lambda (need) (if horizontal (car need) (cdr need))))
                    (least (ghostel-tmux--least-size (cdar placed) horizontal))
                    (whole (region-of node))
                    (scale (if whole
                               (min 1.0 (/ (float (funcall size whole))
                                           (max 1 (funcall dimension
                                                           (ghostel-tmux--layout-need
                                                            node #'window-of)))))
                             1.0)))
               (cl-loop
                for (kid . rest) on (nth 5 node) while rest
                with used = 0 do
                (let ((region (region-of kid)))
                   (when (and region
                              (not (eq region root))
                              (window-next-sibling region)
                              (cl-every (lambda (w) (rassq w placed))
                                        (ghostel-tmux--window-leaves
                                         (window-next-sibling region))))
                     (let* ((need (min (funcall dimension
                                                (ghostel-tmux--layout-need
                                                 kid #'window-of))
                                       (- (funcall size whole) used
                                          (ghostel-tmux--layout-reserve
                                           rest #'window-of horizontal scale least))))
                            ;; Only the next sibling may give or take.
                            (delta (max (- least (funcall size region))
                                        (min (- need (funcall size region))
                                             (- (funcall size (window-next-sibling
                                                               region))
                                                least)))))
                       (unless (zerop delta)
                         (ignore-errors
                           (adjust-window-trailing-edge region delta horizontal t)))))
                   (when region
                     (setq used (+ used (funcall size region))))))
               (mapc #'fit (nth 5 node))))))
      (when root
        (ghostel-tmux--with-tile-resizing
          (fit node))))))

(defun ghostel-tmux--tile-build (wid window)
  "Show the panes of tmux window WID in WINDOW, split to its layout.
Runs in the controller.  A pane without a buffer is left out.  WINDOW is
selected afterwards when it was before, on the pane tmux has active, or
on the pane it showed."
  (save-current-buffer
    (let* ((controller (current-buffer))
           (entry (gethash wid ghostel-tmux--windows))
           (tree (plist-get entry :tree))
           (selected (eq window (selected-window)))
           (shown (buffer-local-value 'ghostel-tmux--pane-id (window-buffer window)))
           (panes (mapcar (lambda (rec)
                            (cons (car rec) (gethash (car rec) ghostel-tmux--panes)))
                          (ghostel-tmux--layout-leaves tree)))
           (shown-tree (ghostel-tmux--layout-prune
                        tree (lambda (pid) (buffer-live-p (alist-get pid panes)))))
           (placed (and shown-tree
                        (let ((window-combination-limit t))
                          (ghostel-tmux--leave-tile window)
                          (ghostel-tmux--with-tile-resizing
                            (ghostel-tmux--tile-place shown-tree window panes
                                                      (list controller wid)))))))
      (when placed
        (setq ghostel-tmux--tiles-exist t)
        (ghostel-tmux--add-tile-hooks)
        (puthash wid (thread-first entry
                                   (plist-put :tile-tree tree)
                                   (plist-put :tiled (mapcar #'car placed))
                                   (plist-put :swap-order nil))
                 ghostel-tmux--windows)
        (ghostel-tmux--tile-tab-lines (mapcar #'cdr placed))
        (ghostel-tmux--tile-fit tree placed)
        (ghostel-tmux--note-tile-size wid (mapcar #'cdr placed))
        (when selected
          (when-let* ((target (or (alist-get (and (eql wid ghostel-tmux--current-window-id)
                                                  ghostel-tmux--active-pane-id)
                                             placed)
                                  (alist-get shown placed)
                                  (cdar placed))))
            (select-window target))))
      placed)))

(defun ghostel-tmux--tile-collapse (windows &optional keep)
  "Delete WINDOWS of a tile but KEEP, which takes their space.
KEEP defaults to the selected window among them, also the one selected
before the minibuffer or by their frame, or the first.  It leaves the
tile, and is selected when one of WINDOWS was.  Return KEEP."
  (save-current-buffer
    (let ((keep (or keep
                    (cl-find-if (lambda (w) (memq w windows))
                                (list (selected-window)
                                      (minibuffer-selected-window)
                                      (and windows
                                           (frame-selected-window
                                            (window-frame (car windows))))))
                    (car windows)))
          (selected (memq (selected-window) windows)))
      (dolist (window windows)
        (unless (eq window keep)
          (when (window-live-p window)
            ;; A window brought back, as by `winner-undo', is of no tile.
            (ghostel-tmux--leave-tile window)
            (ignore-errors (delete-window window)))))
      (when (window-live-p keep)
        (ghostel-tmux--leave-tile keep)
        (when selected
          (select-window keep)))
      keep)))

(defun ghostel-tmux--tile-pids (windows)
  "Return the sorted pane ids of the tile WINDOWS."
  (sort (mapcar #'ghostel-tmux--tile-pid windows)
        #'<))

(defun ghostel-tmux--tile-intact-p (wid windows)
  "Return non-nil when WINDOWS show the tile of WID as its layout is.
They are on one frame, have the panes the tile was built with, are of
the layout's shape, and no other window, such as one split off a
window of the tile, is among them; `ghostel-tmux--tile-order' compares
the order of their panes.  A tile built without some of the layout's
panes is intact only as long as the layout stays the same."
  (let* ((entry (gethash wid ghostel-tmux--windows))
         (tree (plist-get entry :tree))
         (tiled (sort (copy-sequence (plist-get entry :tiled)) #'<)))
    (and (ghostel-tmux--tile-root windows)
         (equal (ghostel-tmux--layout-shape tree)
                (ghostel-tmux--layout-shape (plist-get entry :tile-tree)))
         (equal (ghostel-tmux--tile-pids windows) tiled)
         (cl-every (lambda (w) (memq w windows))
                   (ghostel-tmux--window-leaves (ghostel-tmux--tile-root windows)))
         (or (equal tiled (sort (mapcar #'car (ghostel-tmux--layout-leaves tree)) #'<))
             (equal tree (plist-get entry :tile-tree))))))

(defun ghostel-tmux--tile-ready-p (wid windows)
  "Return non-nil when the tile WINDOWS of WID are as its layout is, in order."
  (and (ghostel-tmux--tile-intact-p wid windows)
       (not (ghostel-tmux--tile-order wid windows))))

(defun ghostel-tmux--tile-placed (windows)
  "Return the alist of (PID . WINDOW) of the tile WINDOWS."
  (mapcar (lambda (w) (cons (ghostel-tmux--tile-pid w) w))
          windows))

(defun ghostel-tmux--tile-tab-lines (windows)
  "Give the top row of the tile WINDOWS a tab line, and the others none.
A stacked window's mode line takes tmux's separator row."
  (let ((top (window-pixel-top (ghostel-tmux--tile-root windows))))
    (dolist (w windows)
      (let ((format (and (> (window-pixel-top w) top) 'none)))
        (unless (eq (window-parameter w 'tab-line-format) format)
          (set-window-parameter w 'tab-line-format format))))))

(defun ghostel-tmux--tile-adopt (windows)
  "Make the tile WINDOWS the windows of the panes they show, if they show all.
A swap of buffers between windows of the tile moves its panes.  Return
non-nil when one moved."
  (let ((shown (mapcar (lambda (w)
                         (let ((pid (buffer-local-value 'ghostel-tmux--pane-id
                                                        (window-buffer w))))
                           (and pid
                                (eq (window-buffer w) (gethash pid ghostel-tmux--panes))
                                pid)))
                       windows))
        (moved nil))
    (when (and (not (memq nil shown))
               (equal (sort (copy-sequence shown) #'<) (ghostel-tmux--tile-pids windows)))
      (cl-mapc (lambda (w pid)
                 (unless (eql pid (ghostel-tmux--tile-pid w))
                   (setq moved t)
                   (set-window-parameter w 'ghostel-tmux-tile
                                         (append (butlast (window-parameter
                                                           w 'ghostel-tmux-tile))
                                                 (list pid)))))
               windows shown))
    moved))

(defun ghostel-tmux--layout-order (wid pids)
  "Return the panes PIDS in the order tmux window WID has them.
That is the order once the swaps on their way are done."
  (let ((entry (gethash wid ghostel-tmux--windows)))
    (or (plist-get entry :swap-order)
        (cl-remove-if-not (lambda (pid) (memq pid pids))
                          (mapcar #'car (ghostel-tmux--layout-leaves
                                         (plist-get entry :tree)))))))

(defun ghostel-tmux--tile-order (wid windows)
  "Compare the order of the panes of the tile WINDOWS with tmux window WID's.
Return nil when tmux has them in that order, `swapping' while swaps on
their way give them that order, see `ghostel-tmux--tile-swap', else
`other'."
  (let ((want (mapcar #'ghostel-tmux--tile-pid windows)))
    (cond
     ((not (equal want (ghostel-tmux--layout-order wid want))) 'other)
     ((plist-get (gethash wid ghostel-tmux--windows) :swap-order) 'swapping))))

(defun ghostel-tmux--swap-commands (slots want &optional active)
  "Return the `swap-pane' commands that turn the pane order SLOTS into WANT.
The ACTIVE pane goes first, as `swap-pane -d' makes the source pane
active when the target was."
  (let ((slots (copy-sequence slots))
        (commands nil))
    (cl-loop for pid in want for i from 0 do
             (let ((there (nth i slots)))
               (unless (eql there pid)
                 (push (if (eql pid active)
                           (format "swap-pane -d -s %%%d -t %%%d" pid there)
                         (format "swap-pane -d -s %%%d -t %%%d" there pid))
                       commands)
                 (setf (nth (cl-position pid slots) slots) there
                       (nth i slots) pid))))
    (nreverse commands)))

(defun ghostel-tmux--tile-swap (wid windows)
  "Ask tmux to order the panes of window WID as the tile WINDOWS show them.
Runs in the controller, for a tile reordered in Emacs, as by
`window-swap-states'.  The swaps follow those on their way; until the
query after them is answered, :swap-order is the order they give."
  (let* ((entry (gethash wid ghostel-tmux--windows))
         (want (mapcar #'ghostel-tmux--tile-pid windows))
         (query (1+ (or (plist-get entry :swap-query) 0))))
    (dolist (command (ghostel-tmux--swap-commands
                      (ghostel-tmux--layout-order wid want) want
                      (if (eql wid ghostel-tmux--current-window-id)
                          ghostel-tmux--active-pane-id
                        (plist-get entry :active-pane))))
      (ghostel-tmux--queue-command command))
    (puthash wid (thread-first entry
                               (plist-put :swap-order want)
                               (plist-put :swap-query query))
             ghostel-tmux--windows)
    (ghostel-tmux--query-swaps wid query 2)))

(defun ghostel-tmux--query-swaps (wid query tries)
  "Ask for the layout of tmux window WID after the swaps of generation QUERY.
Runs in the controller.  Its answer ends the wait for them, as does a
failure TRIES times."
  (ghostel-tmux--queue-command
   (format "display-message -p -t @%d '#{window_visible_layout}'" wid)
   (lambda (output)
     (when-let* ((entry (gethash wid ghostel-tmux--windows)))
       (when (eql (plist-get entry :swap-query) query)
         (if (and (null output) (> tries 1))
             (ghostel-tmux--query-swaps wid query (1- tries))
           (puthash wid (plist-put entry :swap-order nil) ghostel-tmux--windows)
           (when output
             (ghostel-tmux--show-layout
              wid (ghostel-tmux--parse-layout-tree (string-trim output))))))))))

(defun ghostel-tmux--forget-swaps (wid)
  "Stop waiting for the swaps of tmux window WID on their way."
  (when-let* ((entry (gethash wid ghostel-tmux--windows)))
    (puthash wid (thread-first entry
                               (plist-put :swap-order nil)
                               (plist-put :swap-query
                                          (1+ (or (plist-get entry :swap-query) 0))))
             ghostel-tmux--windows)))

(defun ghostel-tmux--tile-update (wid)
  "Show tmux window WID as its layout says, in its tile or a window of it.
Runs in the controller.  A tile of another shape is built again in its
space, kept for a lost pane in `ghostel-tmux--auto-host-window' when it
is part of it.  Without a tile, a window showing a pane of a WID of several
panes gets one.  Waits for the minibuffer to close, see `ghostel-tmux--hold'."
  (let* ((controller (current-buffer))
         (entry (gethash wid ghostel-tmux--windows))
         (tree (plist-get entry :tree))
         (windows (ghostel-tmux--tile-windows controller wid))
         (keep (car (memq ghostel-tmux--auto-host-window windows))))
    (cond
     ((ghostel-tmux--hold #'ghostel-tmux--tile-update wid))
     ((null tree)
      (when windows
        (ghostel-tmux--tile-collapse windows keep)))
     ;; While swaps are on their way, the query after them settles the
     ;; tile; panes in another order otherwise were swapped in tmux.
     ((and windows (ghostel-tmux--tile-intact-p wid windows)
           (or (plist-get entry :swap-order)
               (not (ghostel-tmux--tile-order wid windows))))
      (unless (plist-get entry :swap-order)
        (ghostel-tmux--tile-fit tree (ghostel-tmux--tile-placed windows))
        (ghostel-tmux--note-tile-size wid windows)
        (ghostel-tmux--tile-sync-size wid)))
     (windows
      (ghostel-tmux--tile-build wid (ghostel-tmux--tile-collapse windows keep))
      (ghostel-tmux--tile-sync-size wid))
     ((cdr (ghostel-tmux--layout-leaves tree))
      (when-let* ((window (ghostel-tmux--pane-window-of wid)))
        (ghostel-tmux--tile-build wid window)
        (ghostel-tmux--tile-sync-size wid))))))

(defun ghostel-tmux--settle-layout (wid)
  "Ask tmux for the visible layout of window WID, then show WID as it says.
Runs in the controller after `%layout-change', which can be stale: tmux
resizing a zoomed window reports it unzoomed on the way.  One query at a
time: a layout that changes while it is on its way is in its reply."
  (let ((entry (gethash wid ghostel-tmux--windows)))
    (when (and entry (not (plist-get entry :settling)))
      (puthash wid (plist-put entry :settling t) ghostel-tmux--windows)
      (ghostel-tmux--queue-command
       (format "display-message -p -t @%d '#{window_visible_layout}'" wid)
       (lambda (output)
         (when-let* ((entry (gethash wid ghostel-tmux--windows)))
           (puthash wid (thread-first entry
                                      (plist-put :settling nil)
                                      (plist-put :resize-pending
                                                 (and output
                                                      (plist-get entry :resize-pending))))
                    ghostel-tmux--windows)
           (when output
             (ghostel-tmux--show-layout
              wid (ghostel-tmux--parse-layout-tree (string-trim output))))))))))

(defun ghostel-tmux--show-layout (wid tree)
  "Show tmux window WID laid out as TREE, its visible layout.
Runs in the controller.  The panes of its tile take tmux's sizes.  A
tile resized in Emacs while TREE was on its way asks tmux for the sizes
of its windows instead of fitting them."
  (let* ((entry (gethash wid ghostel-tmux--windows))
         (pending (plist-get entry :resize-pending))
         (swapping (plist-get entry :swap-order))
         (windows (ghostel-tmux--tile-windows (current-buffer) wid)))
    (puthash wid (thread-first entry
                               (plist-put :tree tree)
                               (plist-put :resize-pending (and swapping pending)))
             ghostel-tmux--windows)
    (if (and pending (not swapping) windows (ghostel-tmux--tile-ready-p wid windows)
             (equal (ghostel-tmux--tile-size windows) (plist-get entry :tile-size))
             (ghostel-tmux--tile-resize-commands wid windows))
        (ghostel-tmux--tile-resize-one wid windows)
      (ghostel-tmux--tile-update wid)))
  (when (ghostel-tmux--tile-windows (current-buffer) wid)
    (pcase-dolist (`(,pid ,w ,h) (ghostel-tmux--layout-leaves tree))
      (when-let* ((buf (gethash pid ghostel-tmux--panes)))
        (ghostel-tmux--set-pane-size buf (cons w h))))))

(defun ghostel-tmux--pane-window-of (wid)
  "Return a visible window outside any tile showing a pane of tmux window WID.
The selected window comes first."
  (let ((controller (current-buffer)))
    (cl-find-if (lambda (window)
                  (let ((buf (window-buffer window)))
                    (and (not (ghostel-tmux--tile-leaf window))
                         (eq (buffer-local-value 'ghostel-tmux--controller-buffer buf)
                             controller)
                         (eql (buffer-local-value 'ghostel-tmux--window-id buf) wid))))
                (cons (selected-window)
                      (window-list-1 nil 'no-mini 'visible)))))

(defun ghostel-tmux--tile-sync-size (wid)
  "Report what the tile of tmux window WID can show with `refresh-client -C'.
Runs in the controller; only for the session's current window, whose size the
client sets, only for a tile as its layout is, and only when the size changed.
As when ghostel refits a terminal to its windows, fewer rows while the
minibuffer is active do not count.  Return the size sent, or nil."
  (when (and (eql wid ghostel-tmux--current-window-id)
             (ghostel-tmux--controller-live-p))
    (when-let* ((windows (ghostel-tmux--tile-windows (current-buffer) wid))
                (tree (plist-get (gethash wid ghostel-tmux--windows) :tree))
                ((not (plist-get (gethash wid ghostel-tmux--windows) :settling)))
                ((ghostel-tmux--tile-ready-p wid windows)))
      (let* ((placed (ghostel-tmux--tile-placed windows))
             (capacity (ghostel-tmux--layout-capacity
                        tree (lambda (pid) (alist-get pid placed))))
             (size (cons (max 1 (car capacity)) (max 1 (cdr capacity)))))
        (unless (or (equal size ghostel-tmux--client-size)
                    (and (active-minibuffer-window)
                         (eql (car size) (car ghostel-tmux--client-size))))
          (setq ghostel-tmux--client-size size)
          (ghostel-tmux--queue-command
           (format "refresh-client -C %d,%d" (car size) (cdr size)))
          size)))))

(defun ghostel-tmux--add-tile-hooks ()
  "Check the tiles when windows change, and follow their selection."
  (add-hook 'window-buffer-change-functions #'ghostel-tmux--window-change)
  (add-hook 'window-size-change-functions #'ghostel-tmux--window-change)
  (add-hook 'window-selection-change-functions #'ghostel-tmux--selection-change))

(defun ghostel-tmux--remove-tile-hooks ()
  "Stop checking the tiles when windows change, and following their selection."
  (remove-hook 'window-buffer-change-functions #'ghostel-tmux--window-change)
  (remove-hook 'window-size-change-functions #'ghostel-tmux--window-change)
  (remove-hook 'window-selection-change-functions #'ghostel-tmux--selection-change))

(defun ghostel-tmux--selection-change (frame)
  "Ask tmux for the pane of the selected window, when it is of a tile.
The window must be FRAME's and show its own pane; the tile's size goes
with it, as its tmux window becomes the current one.  Coming back from
a minibuffer that closed selects nothing while calls of the controller
are held back, see `ghostel-tmux--hold': they show what tmux has."
  (let ((window (selected-window))
        (old (frame-old-selected-window frame)))
    (when-let* ((tile (and (eq (window-frame window) frame)
                           (ghostel-tmux--tile-leaf window))))
      (with-current-buffer (nth 0 tile)
        (when (and ghostel-tmux--panes
                   (eq (window-buffer window) (gethash (nth 2 tile) ghostel-tmux--panes))
                   (ghostel-tmux--controller-live-p)
                   (not (and (assq (current-buffer) ghostel-tmux--held-calls)
                             (window-live-p old)
                             (window-minibuffer-p old)
                             (not (active-minibuffer-window)))))
          (ghostel-tmux--select-pane (nth 1 tile) (nth 2 tile))
          (ghostel-tmux--tile-sync-size (nth 1 tile)))))))

(defun ghostel-tmux--window-change (_frame)
  "Check the tiles after a change of windows, when there may be one."
  (when ghostel-tmux--tiles-exist
    (ghostel-tmux--schedule-tile-check)))

(defun ghostel-tmux--schedule-tile-check ()
  "Run `ghostel-tmux--tile-check' from a timer, once for any number of calls."
  (unless (timerp ghostel-tmux--tile-timer)
    (setq ghostel-tmux--tile-timer
          (run-at-time 0 nil #'ghostel-tmux--tile-check))))

(defun ghostel-tmux--tile-check ()
  "Keep each tile in line with its windows and tmux.
A tile that lost windows is built again in its space; one left with a
single window of a tmux window of several panes zooms that pane
instead.  The others report their size and fit their windows again.
Without `ghostel-tmux-split-windows', windows leave their tiles."
  (setq ghostel-tmux--tile-timer nil)
  (dolist (window (ghostel-tmux--all-windows))
    (when (and (window-parameter window 'ghostel-tmux-tile)
               (not (and ghostel-tmux-split-windows (ghostel-tmux--tile-leaf window))))
      (ghostel-tmux--leave-tile window)))
  (when ghostel-tmux-split-windows
    (ghostel-tmux--recover-members))
  (let ((tiles (ghostel-tmux--tiles)))
    (setq ghostel-tmux--tiles-exist (and tiles t))
    (pcase-dolist (`((,controller . ,wid) . ,windows) tiles)
      (with-current-buffer controller
        (ghostel-tmux--tile-check-one wid windows)))))

(defun ghostel-tmux--tile-check-one (wid windows)
  "Check the tile of WINDOWS showing tmux window WID; see `--tile-check'.
A tile waiting for its layout, see `--settle-layout', is left alone but
for its panes moved between its windows, which move in tmux too, see
`--tile-swap'.  A window of the tile resized in Emacs while the tile
kept its size in pixels, as by dragging a divider, resizes its pane,
see `--tile-resize'.  A tile that is not as its layout says waits for
the minibuffer to close, see `ghostel-tmux--hold'."
  (let* ((entry (gethash wid ghostel-tmux--windows))
         (tree (plist-get entry :tree))
         (settling (plist-get entry :settling)))
    (when (and (ghostel-tmux--tile-adopt windows)
               (memq (selected-window) windows))
      (ghostel-tmux--select-pane wid (ghostel-tmux--tile-pid (selected-window))))
    (cond
     ((and (or (null tree)
               (not (or settling (ghostel-tmux--tile-intact-p wid windows))))
           (ghostel-tmux--hold #'ghostel-tmux--schedule-tile-check)))
     ((null tree)
      (dolist (window windows)
        (ghostel-tmux--leave-tile window)))
     ((not (ghostel-tmux--tile-intact-p wid windows))
      (cond
       (settling)
       ((and (null (cdr windows)) (cdr (plist-get entry :tiled)))
        (let ((pid (ghostel-tmux--tile-pid (car windows))))
          (if (eq (window-buffer (car windows)) (gethash pid ghostel-tmux--panes))
              (ghostel-tmux--ensure-pane-zoomed wid pid)
            (ghostel-tmux--leave-tile (car windows)))))
       (t
        (ghostel-tmux--tile-build wid (ghostel-tmux--tile-collapse windows))
        (ghostel-tmux--tile-sync-size wid))))
     (t
      (ghostel-tmux--tile-tab-lines windows)
      (let ((order (ghostel-tmux--tile-order wid windows))
            (same (equal (ghostel-tmux--tile-size windows)
                         (plist-get entry :tile-size))))
        (cond
         ((eq order 'other)
          (ghostel-tmux--tile-swap wid windows))
         (order
          ;; Windows resized meanwhile resize their panes once the
          ;; swaps are done.
          (ghostel-tmux--note-tile-size wid windows)
          (when (and same (ghostel-tmux--tile-resize-commands wid windows))
            (puthash wid (plist-put (gethash wid ghostel-tmux--windows) :resize-pending t)
                     ghostel-tmux--windows)))
         (settling)
         (t
          (ghostel-tmux--note-tile-size wid windows)
          ;; A new size brings a layout, which fits the windows.
          (unless (ghostel-tmux--tile-sync-size wid)
            (if (and same (ghostel-tmux--tile-resize-commands wid windows))
                (ghostel-tmux--schedule-tile-resize)
              (ghostel-tmux--tile-fit tree (ghostel-tmux--tile-placed windows)))))))))))

(defvar ghostel-tmux--resize-timer nil
  "Idle timer of a pending `ghostel-tmux--tile-resize'.")

(defun ghostel-tmux--tile-size (windows)
  "Return (WIDTH . HEIGHT) in pixels of the tile WINDOWS."
  (let ((root (ghostel-tmux--tile-root windows)))
    (cons (window-pixel-width root) (window-pixel-height root))))

(defun ghostel-tmux--note-tile-size (wid windows)
  "Record the pixel size of the tile WINDOWS of tmux window WID."
  (puthash wid (plist-put (gethash wid ghostel-tmux--windows) :tile-size
                          (ghostel-tmux--tile-size windows))
           ghostel-tmux--windows))

(defun ghostel-tmux--edge-pane (node horizontal)
  "Return a pane of layout NODE whose resize moves the edge of NODE, or nil.
tmux resizes the nearest part of a split side by side, with HORIZONTAL, or
stacked above the pane: that of NODE, with only splits the other way in between"
  (cond
   ((eq (car node) 'pane) (nth 5 node))
   ((eq (car node) (if horizontal 'v 'h))
    (cl-some (lambda (kid) (ghostel-tmux--edge-pane kid horizontal)) (nth 5 node)))))

(defun ghostel-tmux--layout-resize-commands (node window-of)
  "Return the `resize-pane' commands giving layout NODE its windows' sizes.
WINDOW-OF returns the window of a pane.  Each part of a split other than
the last whose size differs gets one, outer splits first; a part no pane
can resize, in front of the last, resizes the last instead."
  (unless (eq (car node) 'pane)
    (let ((horizontal (eq (car node) 'h))
          (kids (nth 5 node))
          (commands nil))
      (cl-flet ((size (kid)
                  (let ((capacity (ghostel-tmux--layout-capacity kid window-of)))
                    (if horizontal (car capacity) (cdr capacity))))
                (command (pid size)
                  (format "resize-pane -t %%%d -%s %d"
                          pid (if horizontal "x" "y") (max 1 size))))
        (cl-loop for (kid next) on kids while next do
                 (let ((size (size kid)))
                   (unless (= size (if horizontal (nth 1 kid) (nth 2 kid)))
                     (let ((pid (ghostel-tmux--edge-pane kid horizontal))
                           (last-pid (and (null (cdr (memq next kids)))
                                          (ghostel-tmux--edge-pane next horizontal))))
                       (cond
                        (pid (push (command pid size) commands))
                        (last-pid (push (command last-pid (size next)) commands))))))))
      (append (nreverse commands)
              (mapcan (lambda (kid) (ghostel-tmux--layout-resize-commands kid window-of))
                      kids)))))

(defun ghostel-tmux--tile-resize-commands (wid windows)
  "Return the `resize-pane' commands for the tile WINDOWS of tmux window WID.
Runs in the controller.  Only for the session's current window, whose
size the tile has, outside the minibuffer, and when every window shows
its pane, and the layout and the tile are both of the size tmux was
told: a layout from before is not what the windows are to match."
  (let* ((tree (plist-get (gethash wid ghostel-tmux--windows) :tree))
         (placed (ghostel-tmux--tile-placed windows))
         (window-of (lambda (pid) (alist-get pid placed))))
    (when (and tree
               (eql wid ghostel-tmux--current-window-id)
               (not (active-minibuffer-window))
               (cl-every (lambda (leaf)
                           (eq (window-buffer (cdr leaf))
                               (gethash (car leaf) ghostel-tmux--panes)))
                         placed)
               (equal (cons (nth 1 tree) (nth 2 tree)) ghostel-tmux--client-size)
               (equal (ghostel-tmux--layout-capacity tree window-of)
                      ghostel-tmux--client-size))
      (ghostel-tmux--layout-resize-commands tree window-of))))

(defun ghostel-tmux--schedule-tile-resize ()
  "Run `ghostel-tmux--tile-resize' once Emacs is idle a moment.
A drag of a divider resizes the windows many times; tmux gets the end."
  (when (timerp ghostel-tmux--resize-timer)
    (cancel-timer ghostel-tmux--resize-timer))
  (setq ghostel-tmux--resize-timer
        (run-with-idle-timer 0.3 nil #'ghostel-tmux--tile-resize)))

(defun ghostel-tmux--tile-resize ()
  "Ask tmux to give the panes of each tile the sizes of their windows.
A tile waiting for its layout or swaps does so when the layout is there,
see `ghostel-tmux--show-layout'."
  (setq ghostel-tmux--resize-timer nil)
  (pcase-dolist (`((,controller . ,wid) . ,windows) (ghostel-tmux--tiles))
    (with-current-buffer controller
      (let ((entry (gethash wid ghostel-tmux--windows)))
        (if (or (plist-get entry :settling) (plist-get entry :swap-order))
            (puthash wid (plist-put entry :resize-pending t) ghostel-tmux--windows)
          (ghostel-tmux--tile-resize-one wid windows))))))

(defun ghostel-tmux--tile-resize-one (wid windows)
  "Send the `resize-pane' commands of the tile WINDOWS of tmux window WID.
Runs in the controller.  The layout they bring settles the tile; one
tmux refuses fits the windows to tmux's sizes."
  (when (and (ghostel-tmux--controller-live-p)
             (ghostel-tmux--tile-ready-p wid windows))
    (let ((controller (current-buffer)))
      (dolist (command (ghostel-tmux--tile-resize-commands wid windows))
        (ghostel-tmux--queue-command
         command
         (lambda (output)
           (unless output
             (when-let* ((windows (ghostel-tmux--tile-windows controller wid)))
               (ghostel-tmux--tile-fit
                (plist-get (gethash wid ghostel-tmux--windows) :tree)
                (ghostel-tmux--tile-placed windows))))))))))

(defun ghostel-tmux--split-windows-changed ()
  "Show the panes in the windows as `ghostel-tmux-split-windows' now says."
  (pcase-dolist (`(,_tile . ,windows) (ghostel-tmux--tiles))
    (ghostel-tmux--tile-collapse windows))
  (dolist (window (ghostel-tmux--all-windows))
    (let ((buf (window-buffer window)))
      (when (buffer-live-p (buffer-local-value 'ghostel-tmux--controller-buffer buf))
        (ghostel-tmux--apply-pane-size buf window t)))))

(defun ghostel-tmux--tile-dissolve (controller &optional collapse)
  "Let the windows of CONTROLLER's tiles go.
With COLLAPSE, those showing a pane collapse to one.
Return the windows that remain of them."
  (let (kept)
    (pcase-dolist (`((,owner . ,_wid) . ,windows) (ghostel-tmux--tiles))
      (when (eq owner controller)
        (let ((panes (and collapse
                          (cl-remove-if-not
                           (lambda (w)
                             (eq (buffer-local-value 'ghostel-tmux--controller-buffer
                                                     (window-buffer w))
                                 controller))
                           windows))))
          (dolist (window windows)
            (unless (memq window panes)
              (ghostel-tmux--leave-tile window)
              (push window kept)))
          (when panes
            (push (ghostel-tmux--tile-collapse panes) kept)))))
    kept))


;; Tab line

;; Without `ghostel-tmux-split-windows', `tab-line-mode' lists the pane
;; buffers.  With it, the tab line has a tab per tmux window, and a tile
;; has one bar over its top row of windows, each showing its part.

(defun ghostel-tmux--tab-line-tabs ()
  "Return tabs (sibling pane buffers) for `tab-line-mode' in this buffer."
  (when (buffer-live-p ghostel-tmux--controller-buffer)
    (ghostel-tmux--pane-list ghostel-tmux--controller-buffer)))

(defvar ghostel-tmux--window-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-line down-mouse-1] #'ignore)
    (define-key map [tab-line mouse-1] #'ghostel-tmux--window-tab-select)
    (define-key map [tab-line mouse-2] #'ghostel-tmux--window-tab-select)
    map)
  "Keymap of the tab of a tmux window.")

(defun ghostel-tmux--window-tabs (wid width)
  "Return the tabs of this controller's tmux windows, WID's current.
The bar is padded to WIDTH columns."
  (let* ((tabs (mapconcat
                (lambda (w)
                  (propertize (concat " " (or (plist-get (gethash w ghostel-tmux--windows)
                                                         :name)
                                              "")
                                      " ")
                              'face (if (eql w wid) 'tab-line-tab-current
                                      'tab-line-tab-inactive)
                              'mouse-face 'tab-line-highlight
                              'keymap ghostel-tmux--window-tab-map
                              'help-echo "mouse-1: show this tmux window"
                              'ghostel-tmux-window w))
                ghostel-tmux--window-order
                (propertize " " 'face 'tab-line)))
         (pad (- width (string-width tabs))))
    (if (> pad 0)
        (concat tabs (propertize (make-string pad ?\s) 'face 'tab-line))
      tabs)))

(defun ghostel-tmux--tab-line-columns (window root)
  "Return the columns of the tab line of WINDOW, in the tile ROOT.
A vertical border or divider on WINDOW's right, within ROOT, covers
some of its total width."
  (- (window-total-width window)
     (if (< (+ (window-left-column window) (window-total-width window))
            (+ (window-left-column root) (window-total-width root)))
         (if (display-graphic-p)
             (ceiling (window-right-divider-width window)
                      (frame-char-width (window-frame window)))
           1)
       0)))

(defun ghostel-tmux--tab-line ()
  "Return the tab line of the window redisplay selected to format it.
In split mode, it has a tab per tmux window, and the windows of a
tile's top row each show their part of one bar over the whole tile.
Without, it is `tab-line-mode''s, with a tab per pane."
  (let ((window (selected-window))
        (controller ghostel-tmux--controller-buffer)
        (own ghostel-tmux--window-id))
    (cond
     ((not ghostel-tmux-split-windows) (tab-line-format))
     ((not (buffer-live-p controller)) "")
     (t
      (let* ((tile (ghostel-tmux--tile-leaf window controller))
             (windows (and tile (ghostel-tmux--tile-windows controller (nth 1 tile))))
             (root (or (and windows (ghostel-tmux--tile-root windows)) window))
             (top (window-pixel-top root))
             (left (window-left-column window))
             ;; The columns of the top row's tab lines to the left.
             (start (apply #'+ (mapcar (lambda (w)
                                         (if (and (= (window-pixel-top w) top)
                                                  (< (window-left-column w) left))
                                             (ghostel-tmux--tab-line-columns w root)
                                           0))
                                       windows)))
             (bar (with-current-buffer controller
                    (ghostel-tmux--window-tabs (if tile (nth 1 tile) own)
                                               (window-total-width root))))
             (slice (truncate-string-to-width
                     bar (+ start (ghostel-tmux--tab-line-columns window root))
                     start ?\s)))
        (replace-regexp-in-string "%" (lambda (m) (concat m m)) slice t t))))))

(defun ghostel-tmux--window-tab-select (event)
  "Show the tmux window of the tab clicked in EVENT, in the window clicked."
  (interactive "e")
  (let* ((posn (event-start event))
         (window (posn-window posn))
         (string (posn-string posn))
         (wid (and string
                   (get-text-property (cdr string) 'ghostel-tmux-window (car string))))
         (controller (and (window-live-p window)
                          (buffer-local-value 'ghostel-tmux--controller-buffer
                                              (window-buffer window)))))
    (when (and wid (buffer-live-p controller))
      (with-current-buffer controller
        (ghostel-tmux--show-tmux-window wid window)))))

(defun ghostel-tmux--show-tmux-window (wid window)
  "Show tmux window WID in WINDOW, making it the session's current one.
Runs in the controller.  A tmux window Emacs shows already is selected
where it is, unless the selected window shows it.  The current one is
shown at once; for another, tmux reports the switch, which then shows
it in WINDOW."
  (let* ((entry (gethash wid ghostel-tmux--windows))
         (panes (plist-get entry :panes))
         (pid (or (and (eql wid ghostel-tmux--current-window-id)
                       (car (memq ghostel-tmux--active-pane-id panes)))
                  (car (memq (plist-get entry :active-pane) panes))
                  (car panes)))
         (buf (and pid (gethash pid ghostel-tmux--panes)))
         (shown (or (ghostel-tmux--tile-windows (current-buffer) wid)
                    (and buf (get-buffer-window-list buf nil 'visible)))))
    (cond
     ((not (ghostel-tmux--controller-live-p))
      (user-error "Ghostel-tmux: Controller is not connected"))
     (shown
      (unless (memq (selected-window) shown)
        (save-current-buffer
          (select-window (or (cl-find-if (lambda (w) (eq (window-buffer w) buf)) shown)
                             (car shown)))))
      (unless (eql wid ghostel-tmux--current-window-id)
        (ghostel-tmux--cmd (ghostel-tmux--select-window-command (current-buffer) wid))))
     ((eql wid ghostel-tmux--current-window-id)
      (when buf
        (ghostel-tmux--display-pane-in-window buf window)))
     (t
      (setq ghostel-tmux--auto-host-window window)
      (ghostel-tmux--queue-command
       (ghostel-tmux--select-window-command (current-buffer) wid)
       (lambda (output)
         (unless output
           (when (eq ghostel-tmux--auto-host-window window)
             (setq ghostel-tmux--auto-host-window nil))
           (ghostel-tmux--report-failure "select-window" ghostel-tmux--reply-error))))))))


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
  "Stop ghostel and window changes calling this file before it unloads."
  (when (eq ghostel-control-mode-function #'ghostel-tmux--control)
    (setq ghostel-control-mode-function nil))
  (ghostel-tmux--remove-tile-hooks)
  (remove-hook 'minibuffer-exit-hook #'ghostel-tmux--schedule-held-calls)
  (cancel-function-timers #'ghostel-tmux--run-held-calls)
  (dolist (timer (list ghostel-tmux--tile-timer ghostel-tmux--resize-timer))
    (when (timerp timer)
      (cancel-timer timer)))
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
Forwards to the control-mode parser only when this buffer is currently
currently inside a DCS frame we entered via `ghostel-tmux--dcs-enter'."
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
      (when (and (not closing)
                 (process-live-p (get-buffer-process (current-buffer))))
        (ghostel-send-string "\n" t)))))

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
        ghostel-tmux--active-pane-id nil
        ghostel-tmux--client-size nil
        ghostel-tmux--server-key nil
        ghostel-tmux--detaching nil
        ghostel-tmux--held-calls (assq-delete-all (current-buffer)
                                                  ghostel-tmux--held-calls)))

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
    ;; through `ghostel-send-string'.
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

(defun ghostel-tmux--show-host (pane-buffers)
  "Show this buffer, a host, in the windows showing one of PANE-BUFFERS."
  (let ((host (current-buffer)))
    (walk-windows
     (lambda (window)
       (when (memq (window-buffer window) pane-buffers)
         (with-selected-window window
           (switch-to-buffer host))))
     'no-minibuffer 'visible)))

(defun ghostel-tmux--exit-control-mode (buf)
  "Restore BUF to normal ghostel operation after tmux exited control mode.
Any windows showing pane buffers are returned to the host buffer so the
user lands back at the shell, and the pane buffers are sealed as
read-only snapshots or killed, see `ghostel-tmux-keep-detached-panes'.
Windows that show sealed panes again when a minibuffer has closed are
returned once more, see `ghostel-tmux--hold'."
  (with-current-buffer buf
    (ghostel-tmux--tile-dissolve buf t)
    (let (pane-buffers)
      (when ghostel-tmux--panes
        (maphash (lambda (_pid pane-buf)
                   (when (buffer-live-p pane-buf)
                     (push pane-buf pane-buffers)
                     (ghostel-tmux--seal-pane-buffer pane-buf)))
                 ghostel-tmux--panes))
      (ghostel-tmux--show-host pane-buffers)
      (unless (ghostel-tmux--keep-panes-p ghostel-tmux--detaching)
        (mapc #'kill-buffer pane-buffers))
      (ghostel-tmux--reset-session-state)
      (ghostel-tmux--hold #'ghostel-tmux--show-host pane-buffers))
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
  "End control mode when the auto-takeover host BUF's shell dies.
Runs from `ghostel-exit-functions' (added buffer-locally on control-mode
entry): with the host process gone there is no control channel left, so the
panes are sealed read-only or killed, see `ghostel-tmux--exit-control-mode',
and the host reverts to a plain buffer."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when ghostel-tmux--dcs-active
        (setq ghostel-tmux--dcs-active nil)
        (ghostel-tmux--exit-control-mode buf)))))

(defun ghostel-tmux--restore-identity ()
  "Give this host its `ghostel-identity' back, as a new instance if taken."
  (let ((identity ghostel-tmux--host-identity))
    (when (and (assq 'instance identity)
               (ghostel-find-buffer-by-identity identity))
      (let ((context (assq-delete-all 'instance (copy-alist identity))))
        (setq identity (cons (cons 'instance (ghostel-next-instance context))
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
  "Seal or kill the panes of this auto-takeover host, whose buffer is being killed.
`ghostel-exit-functions' does not run for a buffer that is gone."
  (ghostel-tmux--seal-panes (current-buffer) ghostel-tmux--detaching))


;; Public commands

(defvar-local ghostel-tmux--server nil
  "The tmux program and the arguments that select its server, or nil.
A cons (PROGRAM . ARGS) of `ghostel-tmux--resolve-server'.  A controller
buffer has the server it runs on; the commands bind it around their work.")

(defun ghostel-tmux--socket-args (socket)
  "Return the tmux arguments that select the server of SOCKET.
SOCKET is a value of `ghostel-tmux-socket'; empty string is the default server."
  (cond
   ((or (null socket) (string-empty-p socket)) nil)
   ((file-remote-p socket)
    (user-error "Ghostel-tmux: Socket %s is not a path on the host" socket))
   ((file-name-absolute-p socket)
    ;; `~' expands on the host that runs tmux.
    (list "-S" (file-local-name
                (expand-file-name
                 (concat (file-remote-p default-directory) socket)))))
   (t (list "-L" socket))))

(defun ghostel-tmux--resolve-server (&optional socket)
  "Return the tmux server (PROGRAM . ARGS) for `default-directory'.
These are the connection-local values of `ghostel-tmux-program' and
`ghostel-tmux-socket'.  A non-nil SOCKET overrides the socket."
  (with-connection-local-variables
   (cons ghostel-tmux-program
         (ghostel-tmux--socket-args (or socket ghostel-tmux-socket)))))

(defun ghostel-tmux--read-socket ()
  "With a prefix argument, prompt for a tmux socket and return it, else nil."
  (and current-prefix-arg
       (read-string "tmux socket (name or path, empty for the default): ")))

(defun ghostel-tmux--server-label ()
  "Return a suffix naming the server of `ghostel-tmux--server', or \"\"."
  (if (cdr ghostel-tmux--server)
      (concat " on " (string-join (cdr ghostel-tmux--server) " "))
    ""))

(defun ghostel-tmux--process-file (&rest args)
  "Run tmux with ARGS on `ghostel-tmux--server' and return (STATUS . STDOUT).
tmux's text is UTF-8 whatever the locale."
  (let ((server (or ghostel-tmux--server (ghostel-tmux--resolve-server)))
        (coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix))
    (with-temp-buffer
      (let ((status (apply #'process-file (car server) nil (list t nil) nil
                           "-u" (append (cdr server) args))))
        (cons status (buffer-string))))))

(defun ghostel-tmux--read-session ()
  "Prompt for a tmux session name and return it.
A non-zero exit (no server running) counts as no sessions."
  (let* ((result (ghostel-tmux--process-file
                  "list-sessions" "-F" "#{session_name}"))
         (sessions (and (eql (car result) 0)
                        (split-string (cdr result) "\n" t)))
         (default (car sessions)))
    (cond
     ((null sessions)
      (user-error "Ghostel-tmux: No tmux sessions found%s"
                  (ghostel-tmux--server-label)))
     ((= (length sessions) 1) (car sessions))
     (t (completing-read "tmux session: " sessions nil t nil nil default)))))

(defun ghostel-tmux--check-spawn ()
  "Signal `user-error' when no controller can start in `default-directory'."
  (ghostel-ensure-module)
  ;; A remote shell that cannot `cd' there stays alive at its prompt.
  (unless (file-directory-p default-directory)
    (user-error "Ghostel-tmux: No directory %s" default-directory)))

(defun ghostel-tmux--spawn-controller (session tmux-args)
  "Spawn a `tmux -C' subprocess on a pipe as a controller buffer.
SESSION is the user-visible session label.  TMUX-ARGS is the list
of arguments after \"tmux -C\".  `-u' keeps non-ASCII text intact in
any locale.  A remote `default-directory' runs tmux on that host.
A remote tmux can get a terminal, as from TRAMP's direct async
processes: raw mode keeps it from echoing commands and from cutting
long lines.  tmux is `ghostel-tmux--server', which the controller
buffer keeps.  The selected window waits for tmux's active pane while
it shows the same buffer.  Returns the controller buffer."
  (let* ((server (or ghostel-tmux--server (ghostel-tmux--resolve-server)))
         (command `(,(car server) "-u" ,@(cdr server) "-C" ,@tmux-args))
         (control-buf (generate-new-buffer
                       (format " *ghostel-tmux-control:%s*" session))))
    (condition-case err
        (with-current-buffer control-buf
          (ghostel-tmux-control-mode)
          (setq ghostel-tmux--server server)
          (let* ((proc (make-process
                        :name (format "ghostel-tmux-%s" session)
                        :buffer control-buf
                        :command
                        (if (file-remote-p default-directory)
                            `("sh" "-c" "stty raw -echo 2>/dev/null; exec \"$@\""
                              "sh" ,@command)
                          command)
                        :coding 'binary
                        :connection-type 'pipe
                        :file-handler t
                        :noquery t
                        :filter #'ghostel-tmux--filter
                        :sentinel #'ghostel-tmux--sentinel)))
            ;; TRAMP sets the coding of its connection after `:coding'.
            (set-process-coding-system proc 'binary 'binary)
            ;; The last attach started in a window fills it.
            (dolist (buf (buffer-list))
              (when (and (eq (buffer-local-value 'major-mode buf)
                             'ghostel-tmux-control-mode)
                         (eq (buffer-local-value 'ghostel-tmux--auto-host-window
                                                 buf)
                             (selected-window)))
                (with-current-buffer buf
                  (setq ghostel-tmux--auto-host-window nil))))
            (let ((fill (list #'ghostel-tmux--fill-kept-window (selected-window))))
              (dolist (call ghostel-tmux--held-calls)
                (when (and (equal (cdr call) fill) (buffer-live-p (car call)))
                  (with-current-buffer (car call)
                    (setq ghostel-tmux--auto-host-buffer nil))))
              (setq ghostel-tmux--held-calls
                    (cl-remove fill ghostel-tmux--held-calls
                               :key #'cdr :test #'equal)))
            (setq ghostel-tmux--controller-process proc
                  ghostel-tmux--session-name session
                  ghostel-tmux--auto-host-window (selected-window)
                  ghostel-tmux--auto-host-buffer (window-buffer))))
      ((error quit)
       (kill-buffer control-buf)
       (signal (car err) (cdr err))))
    (message "Ghostel-tmux: Attaching to %s..." session)
    control-buf))

(defun ghostel-tmux--apply-pane-size (pane-buf window &optional defer)
  "Fit PANE-BUF's terminal to WINDOW showing it and tell tmux about the size.
Splitting and tab-line switches both hit this path, so a pane that
was created at default geometry gets corrected the moment it becomes
visible in an Emacs window.  The size is reported even when the grid
already fits, because tmux's client size may still be that of another
pane's window.  tmux is asked to switch to the pane.
With `ghostel-tmux-split-windows', a pane of a tmux window of several
panes gets a tile in WINDOW, built from a timer with DEFER; a pane of a
tmux window that has a tile already keeps tmux's size.  A window of a
tile showing a pane of another tmux window loses its tile.  Without it,
tmux is asked to zoom the pane to the full client size: otherwise a
split leaves half a pane in a full-height Emacs window."
  (when (and (window-live-p window) (buffer-live-p pane-buf))
    (with-current-buffer pane-buf
      (let ((pane-id ghostel-tmux--pane-id)
            (window-id ghostel-tmux--window-id)
            (controller ghostel-tmux--controller-buffer))
        ;; A window state put in WINDOW, as by `window-swap-states',
        ;; dropped its tile.
        (when ghostel-tmux-split-windows
          (ghostel-tmux--recover-members))
        (cond
         ((not ghostel-tmux-split-windows)
          (ghostel-fit-window window)
          (when (and pane-id window-id (buffer-live-p controller))
            (with-current-buffer controller
              (when (ghostel-tmux--controller-live-p)
                (ghostel-tmux--select-pane window-id pane-id)
                (ghostel-tmux--ensure-pane-zoomed window-id pane-id)))))
         ((ghostel-tmux--tile-leaf window controller window-id)
          (ghostel-tmux--schedule-tile-check))
         ((ghostel-tmux--tile-leaf window)
          ;; A pane of another tmux window replaces that window's tile.
          (if defer
              (run-at-time 0 nil #'ghostel-tmux--replace-tile pane-buf window)
            (ghostel-tmux--replace-tile pane-buf window)))
         ((not (buffer-live-p controller)))
         ((ghostel-tmux--tile-windows controller window-id))
         ((cdr (ghostel-tmux--layout-leaves
                (plist-get (gethash window-id (buffer-local-value
                                               'ghostel-tmux--windows controller))
                           :tree)))
          (if defer
              (run-at-time 0 nil #'ghostel-tmux--show-tile pane-buf window)
            (ghostel-tmux--show-tile pane-buf window)))
         (t
          (ghostel-fit-window window)
          (when (and pane-id window-id)
            (with-current-buffer controller
              (when (ghostel-tmux--controller-live-p)
                (ghostel-tmux--select-pane window-id pane-id))))))))))

(defun ghostel-tmux--replace-tile (pane-buf window)
  "Collapse the tile WINDOW is part of into it, to show PANE-BUF there.
Waits for the minibuffer to close, see `ghostel-tmux--hold'."
  (when-let* ((tile (and (eq (window-buffer window) pane-buf)
                         (ghostel-tmux--tile-leaf window))))
    (unless (with-current-buffer (nth 0 tile)
              (ghostel-tmux--hold #'ghostel-tmux--replace-tile pane-buf window))
      (ghostel-tmux--tile-collapse
       (ghostel-tmux--tile-windows (nth 0 tile) (nth 1 tile)) window)
      (ghostel-tmux--apply-pane-size pane-buf window))))

(defun ghostel-tmux--show-tile (pane-buf window)
  "Build a tile in WINDOW for the tmux window of PANE-BUF, which it shows.
tmux is asked to switch to the pane first, which the tile then selects.
Waits for the minibuffer to close, see `ghostel-tmux--hold'."
  (ghostel-tmux--recover-members)
  (when (and (window-live-p window)
             (eq (window-buffer window) pane-buf)
             (not (ghostel-tmux--tile-leaf window)))
    (let ((pane-id (buffer-local-value 'ghostel-tmux--pane-id pane-buf))
          (window-id (buffer-local-value 'ghostel-tmux--window-id pane-buf))
          (controller (buffer-local-value 'ghostel-tmux--controller-buffer pane-buf)))
      (when (buffer-live-p controller)
        (with-current-buffer controller
          (unless (or (ghostel-tmux--tile-windows controller window-id)
                      (ghostel-tmux--hold #'ghostel-tmux--show-tile
                                          pane-buf window))
            (when (ghostel-tmux--controller-live-p)
              (ghostel-tmux--select-pane window-id pane-id))
            (ghostel-tmux--tile-build window-id window)
            (ghostel-tmux--tile-sync-size window-id)))))))

(defun ghostel-tmux--send-selection (controller window-id pane-id)
  "Ask tmux, through CONTROLLER, for PANE-ID in window WINDOW-ID.
The pane goes first, so that `%session-window-changed' finds it recorded
as the window's active pane."
  (ghostel-tmux--cmd (format "select-pane -t %%%d" pane-id))
  (when window-id
    (ghostel-tmux--cmd
     (ghostel-tmux--select-window-command controller window-id))))

(defun ghostel-tmux--select-pane (window-id pane-id &optional force)
  "Ask tmux to switch to PANE-ID in window WINDOW-ID, which Emacs selected.
Does nothing for `ghostel-tmux--active-pane-id', the pane tmux already has
or the one standing in for a floating pane, unless FORCE: answering a report
with a selection would chase a newer selection forever.
The selection replaces a deferred follow."
  (unless (and (eql pane-id ghostel-tmux--active-pane-id) (not force))
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
  "Display PANE-BUF in WINDOW and resize its terminal to fit.
A tile of another tmux window collapses into WINDOW first.  In a tile
of the pane's own tmux window, nothing is shown: the tile shows its panes."
  (let ((tile (ghostel-tmux--tile-leaf window)))
    (when (and tile (buffer-live-p pane-buf))
      (if (and (eq (nth 0 tile) (buffer-local-value 'ghostel-tmux--controller-buffer
                                                    pane-buf))
               (eql (nth 1 tile) (buffer-local-value 'ghostel-tmux--window-id
                                                     pane-buf)))
          (setq window nil)
        (ghostel-tmux--tile-collapse
         (ghostel-tmux--tile-windows (nth 0 tile) (nth 1 tile)) window))))
  (when (and (window-live-p window) (buffer-live-p pane-buf))
    (with-selected-window window
      (switch-to-buffer pane-buf t))
    (ghostel-tmux--apply-pane-size pane-buf window)))

(defun ghostel-tmux--on-window-buffer-change (window)
  "Resize the pane terminal when this buffer becomes WINDOW's content.
Hook for `window-buffer-change-functions': covers tab-line clicks and
any other path that swaps the buffer into a window without going through
`ghostel-tmux--display-pane-in-window'.  From Emacs 31 the hook runs in its
buffer, also for the buffer WINDOW lost; before, in whatever buffer is current."
  (when (and (window-live-p window)
             (or (< emacs-major-version 31)
                 (eq (window-buffer window) (current-buffer))))
    (let ((pane (window-buffer window)))
      (when (buffer-live-p (buffer-local-value
                            'ghostel-tmux--controller-buffer pane))
        (ghostel-tmux--apply-pane-size pane window t)))))

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

;; A bare name is parsed as a target: `proj.0' would be pane 0 of `proj'.
(defun ghostel-tmux--exact-session-target (session)
  "Return the tmux target of the session named exactly SESSION."
  (format "=%s:" session))

(defun ghostel-tmux--session-exists-p (session)
  "Return non-nil when tmux has a session named exactly SESSION."
  (eql 0 (car (ignore-errors
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
(defun ghostel-tmux-attach (session &optional socket)
  "Attach to the tmux SESSION via control mode and materialize its panes.
SESSION is an exact name.  tmux cannot address a name that starts with
`$' or contains `:' this way.  A non-nil SOCKET is used instead of
`ghostel-tmux-socket'; interactively, a prefix argument prompts for it.
Returns the controller buffer before any pane is there: tmux's active
pane takes the selected window once tmux reports it, unless the window
shows another buffer by then."
  (interactive
   (let* ((socket (ghostel-tmux--read-socket))
          (ghostel-tmux--server (ghostel-tmux--resolve-server socket)))
     (list (ghostel-tmux--read-session) socket)))
  (ghostel-tmux--check-spawn)
  (let ((ghostel-tmux--server (ghostel-tmux--resolve-server socket)))
    (ghostel-tmux--spawn-controller session
                                    (ghostel-tmux--attach-args session))))

;;;###autoload
(defun ghostel-tmux-new (session &optional socket)
  "Create or attach to tmux SESSION via control mode.
An existing session is attached like in `ghostel-tmux-attach', as
`new-session -A' reaches it by a raw target;
otherwise `tmux -C new-session -As' creates it.
A non-nil SOCKET is used instead of `ghostel-tmux-socket'.
Interactively, a prefix argument prompts for it.
Returns before any pane is there, like `ghostel-tmux-attach'."
  (interactive
   (let ((socket (ghostel-tmux--read-socket)))
     (list (read-string "tmux session name: " "main") socket)))
  (when (string-empty-p session)
    (user-error "Ghostel-tmux: Empty session name"))
  (ghostel-tmux--check-spawn)
  (let ((ghostel-tmux--server (ghostel-tmux--resolve-server socket)))
    (ghostel-tmux--spawn-controller
     session
     (if (ghostel-tmux--session-exists-p session)
         (ghostel-tmux--attach-args session)
       ;; tmux expands `-s' as a format.
       (list "new-session" "-As" (ghostel-tmux--literal-format session))))))

(defun ghostel-tmux-detach (&optional keep)
  "Detach the current tmux session.
For sessions started via `ghostel-tmux-attach' or `ghostel-tmux-new',
kills the dedicated control-mode subprocess.  For sessions taken over
from an existing ghostel shell (takeover), sends `detach-client' and lets
tmux exit control mode cleanly so the underlying shell continues running.
The pane buffers go, see `ghostel-tmux-keep-detached-panes'; with KEEP
non-nil, interactively a prefix argument, they stay read-only."
  (interactive "P")
  (let ((controller (ghostel-tmux--active-controller)))
    (unless (buffer-live-p controller)
      (user-error "Ghostel-tmux: No active controller for this buffer"))
    (with-current-buffer controller
      (let ((dedicated (derived-mode-p 'ghostel-tmux-control-mode))
            (alive (if keep 'keep t)))
        (when (ghostel-tmux--controller-live-p)
          (setq ghostel-tmux--detaching alive)
          (ghostel-tmux--queue-command "detach-client")
          (setq ghostel-tmux--closing t)
          ;; Give tmux a brief moment to acknowledge the detach (and,
          ;; if another command was in flight, for the queue to reach
          ;; it) so its reply — and the DCS terminator in the
          ;; auto-takeover case — isn't lost to the upcoming
          ;; `delete-process'.
          (let ((proc (or ghostel-tmux--controller-process
                          (get-buffer-process (current-buffer)))))
            (when (process-live-p proc)
              (accept-process-output proc 0.2))))
        (cond
         (dedicated
          ;; Dedicated controller buffer: kill its `tmux -C' subprocess.
          (when ghostel-tmux--controller-process
            (delete-process ghostel-tmux--controller-process))
          (ghostel-tmux--seal-panes controller alive))
         (t
          ;; Auto-takeover host: tmux will exit control mode in response,
          ;; the native handler sees the DCS terminator, and
          ;; `ghostel-tmux--exit-control-mode' tears down pane state.
          nil))))))

(defun ghostel-tmux-next-pane ()
  "Switch to the next tmux pane in this session."
  (interactive)
  (ghostel-tmux--cycle-pane 1))

(defun ghostel-tmux-prev-pane ()
  "Switch to the previous tmux pane in this session."
  (interactive)
  (ghostel-tmux--cycle-pane -1))

(defvar ghostel-tmux--cycle-target nil
  "Pane buffer the last `ghostel-tmux--cycle-pane' asked tmux for.")

(defun ghostel-tmux--cycle-pane (direction &optional window-id)
  "Switch to the next pane in DIRECTION (+1 or -1).
The panes are those of tmux window WINDOW-ID, else of all tmux windows.
A repeated cycle steps on from the pane it last asked for, which is not
shown yet when tmux has to unzoom its window first."
  (let* ((controller ghostel-tmux--controller-buffer)
         (panes (and (buffer-live-p controller)
                     (if window-id
                         (with-current-buffer controller
                           (cl-remove-if-not
                            #'buffer-live-p
                            (mapcar (lambda (pid) (gethash pid ghostel-tmux--panes))
                                    (plist-get (gethash window-id ghostel-tmux--windows)
                                               :panes))))
                       (ghostel-tmux--pane-list controller))))
         (here (if (and (memq last-command
                              '(ghostel-tmux-next-pane ghostel-tmux-prev-pane
                                ghostel-tmux--prefix-next-pane))
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
  "Switch to the pane of PANE-BUF, also across tmux windows.
tmux is asked first.  Then a visible window showing PANE-BUF is
selected, else the selected window shows it, so that keys typed next go
to the pane before tmux reports the switch.  A tile of the pane's tmux
window that lacks it, as a zoomed one, is selected instead, and shows
the pane once tmux's layout arrives."
  (let* ((pid (buffer-local-value 'ghostel-tmux--pane-id pane-buf))
         (wid (buffer-local-value 'ghostel-tmux--window-id pane-buf))
         (controller (buffer-local-value 'ghostel-tmux--controller-buffer
                                         pane-buf))
         (tile (and wid (buffer-live-p controller)
                    (ghostel-tmux--tile-windows controller wid)))
         (window (or (cl-find-if (lambda (w) (eq (window-buffer w) pane-buf)) tile)
                     (get-buffer-window pane-buf 'visible))))
    (when pid
      (unless (buffer-live-p controller)
        (user-error "Ghostel-tmux: No active controller"))
      (with-current-buffer controller
        (when wid
          (ghostel-tmux--cancel-zooming wid pid))
        (ghostel-tmux--select-pane wid pid t))
      (cond
       (window
        (select-window window)
        (unless (ghostel-tmux--tile-leaf window)
          (ghostel-tmux--apply-pane-size pane-buf window)))
       (tile
        (unless (memq (selected-window) tile)
          (select-window (car tile))))
       (t
        (ghostel-tmux--display-pane-in-window pane-buf (selected-window)))))))

(defun ghostel-tmux-switch-pane ()
  "Switch to a sibling pane via `completing-read'.
The choices stay current while the prompt is open, as tmux can rename
a window at any time."
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

(defun ghostel-tmux-restore-split ()
  "Split the selected window's tmux window again as tmux lays it out.
The windows of its split show their panes again, a split that lost
windows is built again, and a zoomed pane is unzoomed.  The tmux
window is that of the split or pane the selected window shows, else
of the first split on its frame.  A split elsewhere moves here."
  (interactive)
  (unless ghostel-tmux-split-windows
    (user-error "Ghostel-tmux: `ghostel-tmux-split-windows' is off"))
  (let* ((window (selected-window))
         (buf (window-buffer window))
         (controller (buffer-local-value 'ghostel-tmux--controller-buffer buf))
         (_ (when (and (buffer-local-value 'ghostel-tmux-pane-mode buf)
                       (not (buffer-live-p controller)))
              (user-error "Ghostel-tmux: This pane is detached")))
         (tile (or (ghostel-tmux--tile-leaf window)
                   (and (buffer-live-p controller)
                        (buffer-local-value 'ghostel-tmux--pane-id buf)
                        (list controller (buffer-local-value 'ghostel-tmux--window-id buf)))
                   (cl-some #'ghostel-tmux--tile-leaf (window-list nil 'no-mini)))))
    (unless tile
      (user-error "Ghostel-tmux: No tmux pane here"))
    (with-current-buffer (nth 0 tile)
      (ghostel-tmux--restore-tile (nth 1 tile) window))))

(defun ghostel-tmux--restore-tile (wid window)
  "Show tmux window WID split as its layout says, in its tile or WINDOW.
Runs in the controller.  WINDOW takes the tile when it is not part of it but
shows a pane of WID; the windows of the tile showing a pane are deleted then."
  (let* ((controller (current-buffer))
         (entry (gethash wid ghostel-tmux--windows))
         (tree (plist-get entry :tree))
         (windows (ghostel-tmux--tile-windows controller wid)))
    (unless tree
      (user-error "Ghostel-tmux: Window @%d has no layout yet" wid))
    (ghostel-tmux--forget-swaps wid)
    (when (and (not (memq window windows))
               (eq (buffer-local-value 'ghostel-tmux--controller-buffer (window-buffer window))
                   controller)
               (eql (buffer-local-value 'ghostel-tmux--window-id (window-buffer window)) wid))
      (dolist (w windows)
        (ghostel-tmux--leave-tile w)
        (when (eq (buffer-local-value 'ghostel-tmux--controller-buffer (window-buffer w))
                  controller)
          (ignore-errors (delete-window w))))
      (setq windows nil))
    (dolist (w windows)
      (when-let* ((pane (gethash (ghostel-tmux--tile-pid w) ghostel-tmux--panes)))
        (unless (eq (window-buffer w) pane)
          (ignore-errors (set-window-buffer w pane)))))
    (cond
     ((null windows)
      (ghostel-tmux--tile-build wid window))
     ((not (ghostel-tmux--tile-ready-p wid windows))
      (ghostel-tmux--tile-build wid (ghostel-tmux--tile-collapse
                                     windows (car (memq window windows))))))
    ;; The visible layout of a zoomed window is the zoomed pane.
    (if (and (plist-get entry :zoomed) (null (cdr (ghostel-tmux--layout-leaves tree))))
        (ghostel-tmux--queue-command
         (format "resize-pane -Z -t %%%d" (car (car (ghostel-tmux--layout-leaves tree)))))
      (ghostel-tmux--selection-change (selected-frame))
      (ghostel-tmux--tile-sync-size wid))))


;; One-shot snapshot of a single pane (no live connection)

;;;###autoload
(defun ghostel-tmux-capture (target &optional socket)
  "Capture TARGET pane (e.g. \"session:window.pane\") into a fresh ghostel buffer.
The buffer renders the captured bytes through a VT engine but is not
connected to a live tmux session.  In a pane buffer of a live session,
the session's tmux captures it.  Otherwise a non-nil SOCKET is used instead
of `ghostel-tmux-socket'; interactively, a prefix argument prompts for it."
  (interactive
   (let ((socket (ghostel-tmux--read-socket)))
     (list (read-string "tmux target (e.g. mysession:0.0 or %3): "
                        (or (and (ghostel-tmux--live-controller)
                                 (ghostel-tmux--prefix-target))
                            ""))
           socket)))
  (ghostel-ensure-module)
  (let ((controller (ghostel-tmux--live-controller))
        (command (list "capture-pane" "-peJC"
                       "-S" (number-to-string (- ghostel-tmux-history-lines))
                       "-t" target)))
    (if (and controller (null socket))
        (with-current-buffer controller
          (ghostel-tmux--queue-command
           (mapconcat #'shell-quote-argument command " ")
           (lambda (output)
             (condition-case err
                 (ghostel-tmux--show-capture target output "")
               (user-error (message "%s" (error-message-string err)))))))
      (let* ((ghostel-tmux--server (ghostel-tmux--resolve-server socket))
             (result (apply #'ghostel-tmux--process-file command)))
        (ghostel-tmux--show-capture target
                                    (and (eql (car result) 0) (cdr result))
                                    (ghostel-tmux--server-label))))))

(defun ghostel-tmux--live-controller ()
  "Return the controller of the current pane buffer while it reaches tmux."
  (let ((controller ghostel-tmux--controller-buffer))
    (and (buffer-live-p controller)
         (with-current-buffer controller
           (and (ghostel-tmux--controller-live-p)
                (not ghostel-tmux--closing)))
         controller)))

(defun ghostel-tmux--show-capture (target output label)
  "Show OUTPUT of `capture-pane -peJC' of TARGET in a fresh buffer.
A nil OUTPUT is a failed capture.  LABEL names the server in errors."
  (unless output
    (user-error "Ghostel-tmux: capture-pane failed for %s%s" target label))
  (let ((rows (string-trim-right output "\n+")))
    (when (string-empty-p rows)
      (user-error "Ghostel-tmux: Empty capture for %s%s" target label))
    (let ((buf (ghostel-make-terminal
                (format "*ghostel-tmux-capture:%s*" target)
                :size (cons ghostel-tmux-default-cols ghostel-tmux-default-rows))))
      (with-current-buffer buf
        (ghostel-tmux-pane-mode 1)
        (ghostel-tmux--drop-tab-line)
        (ghostel-write-output (ghostel-tmux--backfill-rows rows) t)
        (setq ghostel-tmux--initialized t))
      (pop-to-buffer buf))))

(provide 'ghostel-tmux)

;;; ghostel-tmux.el ends here
