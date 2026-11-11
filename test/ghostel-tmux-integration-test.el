;;; ghostel-tmux-integration-test.el --- End-to-end ghostel-tmux test -*- lexical-binding: t; -*-

;;; Commentary:

;; End-to-end integration test for ghostel-tmux.  Spawns a fake `tmux'
;; shell script that emits a hand-crafted but wire-accurate control-mode
;; transcript, then asserts that ghostel-tmux materializes the pane buffer
;; and renders the expected text.  Also feeds a control-mode frame
;; through a terminal and verifies the takeover.  Requires ghostel's
;; native module.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ghostel-tmux)

;; ---------------------------------------------------------------------------
;; Fake tmux fixture
;; ---------------------------------------------------------------------------
;;
;; The wire format below was observed against tmux 3.4 in a docker
;; container.  Lines end with CRLF.  %output payloads use octal escape
;; encoding for control bytes.  The fake replies to each stdin command
;; line with its own `%begin'/`%end' block, mirroring real control-mode
;; pairing (one reply block per command line, in wire order).

(defconst ghostel-tmux-integration-test--script
  "#!/bin/sh
printf '\\033P1000p%%begin 1 0 0\\r\\n'
printf '%%end 1 0 0\\r\\n'
printf '%%session-changed $0 0\\r\\n'
i=1
while IFS= read -r line; do
  i=$((i+1))
  case \"$line\" in
    *alternate_on*)
      printf '%%begin %d %d 1\\r\\n0,0,0,16,1,1,1,0,0,0,1,0,0,0,0,0,0,1,2\\r\\n%%end %d %d 1\\r\\n' $i $i $i $i ;;
    list-windows*)
      printf '%%begin %d %d 1\\r\\n@0\\t%%0\\t1\\tmain\\tbd5b,80x24,0,0,0\\tbd5b,80x24,0,0,0\\r\\n%%end %d %d 1\\r\\n' $i $i $i $i ;;
    capture-pane*)
      printf '%%begin %d %d 1\\r\\nHISTORY_LINE_ONE\\r\\nVISIBLE_PROMPT$ \\r\\n%%end %d %d 1\\r\\n' $i $i $i $i
      printf '%%output %%0 BANNER_FROM_LIVE\\r\\n' ;;
    *)
      printf '%%begin %d %d 1\\r\\n%%end %d %d 1\\r\\n' $i $i $i $i ;;
  esac
done
"
  "Body of the fake tmux executable used by the integration test.")

(defun ghostel-tmux-integration-test--make-fake-tmux ()
  "Write the fake-tmux script to a tempfile and return its path."
  (let ((path (make-temp-file "ghostel-tmux-fake-" nil ".sh")))
    (with-temp-file path
      (insert ghostel-tmux-integration-test--script))
    (set-file-modes path #o755)
    path))

(defmacro ghostel-tmux-integration-test--with-host (spec &rest body)
  "Run BODY in a fresh ghostel buffer with a live terminal attached.
SPEC is (BUFFER TERM)."
  (declare (indent 1))
  (pcase-let ((`(,buffer ,term) spec))
    `(let ((,buffer (generate-new-buffer " *ghostel-tmux-int*")))
       (unwind-protect
           (progn
             (ghostel--init-buffer ,buffer 24 80)
             (with-current-buffer ,buffer
               (let ((,term ghostel--term))
                 ,@body)))
         (when (buffer-live-p ,buffer)
           (kill-buffer ,buffer))))))

(defun ghostel-tmux-integration-test--drain ()
  "Fire the deferred callbacks queued by `ghostel--defer'."
  (accept-process-output nil 0.05))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(ert-deftest ghostel-tmux-integration-attach-renders-pane ()
  "A dedicated controller against fake tmux materializes a rendered pane."
  :tags '(native)
  (let* ((fake (ghostel-tmux-integration-test--make-fake-tmux))
         (ghostel-tmux-program fake)
         (ghostel-tmux-history-lines 100)
         created-buf)
    (unwind-protect
        (progn
          ;; Override the session picker to skip list-sessions.
          (cl-letf (((symbol-function 'ghostel-tmux--read-session)
                     (lambda () "0")))
            (ghostel-tmux-attach "0"))
          ;; Wait until we see a pane buffer created and rendered.
          (let ((deadline (+ (float-time) 5.0)))
            (catch 'done
              (while (< (float-time) deadline)
                (let ((bufs (cl-remove-if-not
                             (lambda (b)
                               (string-prefix-p "*ghostel-tmux:0:"
                                                (buffer-name b)))
                             (buffer-list))))
                  (when (and bufs
                             (with-current-buffer (car bufs)
                               (and ghostel-tmux--initialized
                                    ghostel--term)))
                    (setq created-buf (car bufs))
                    (throw 'done t)))
                (accept-process-output nil 0.1))))
          (should created-buf)
          ;; The capture is on screen, output continues at the cursor of
          ;; the pane's state, and that state's modes are on.  Output
          ;; before the last reply is part of the capture.
          (accept-process-output nil 0.2)
          (with-current-buffer created-buf
            (should (string-match-p
                     "HISTORY_LINE_ONE\nVISIBLE_PROMPT\\$ BANNER_FROM_LIVE\\'"
                     (string-trim-right
                      (ghostel--copy-all-text ghostel--term))))
            (should (ghostel--mode-enabled ghostel--term 2004))))
      (when (file-exists-p fake) (delete-file fake))
      (when (and created-buf (buffer-live-p created-buf))
        (kill-buffer created-buf))
      ;; Kill any controller buffer + child process we left behind.
      (dolist (b (buffer-list))
        (when (string-prefix-p " *ghostel-tmux-control:" (buffer-name b))
          (with-current-buffer b
            (when (and ghostel-tmux--controller-process
                       (process-live-p ghostel-tmux--controller-process))
              (delete-process ghostel-tmux--controller-process)))
          (kill-buffer b))))))

(ert-deftest ghostel-tmux-integration-takeover ()
  "A control-mode frame in a terminal's output takes its buffer over."
  :tags '(native)
  (let ((ghostel-control-mode-function nil)
        (ghostel-tmux-takeover-mode nil))
    (ghostel-tmux-takeover-mode 1)
    (unwind-protect
        (ghostel-tmux-integration-test--with-host (host term)
          (let ((name (buffer-name)))
            (ghostel--write-vt term "\eP1000p%begin 1 1 0\r\n%end 1 1 0\r\n")
            (ghostel-tmux-integration-test--drain)
            (should ghostel-tmux--dcs-active)
            (should (string-prefix-p " *ghostel-tmux-host:" (buffer-name)))
            (ghostel--write-vt term "\e\\")
            (ghostel-tmux-integration-test--drain)
            (should-not ghostel-tmux--dcs-active)
            (should (equal name (buffer-name)))))
      (ghostel-tmux-takeover-mode -1))))

(ert-deftest ghostel-tmux-integration-takeover-name-split-in-a-character ()
  "A session name split inside a character decodes whole in the takeover."
  :tags '(native)
  (let ((ghostel-control-mode-function nil)
        (ghostel-tmux-takeover-mode nil))
    (ghostel-tmux-takeover-mode 1)
    (unwind-protect
        (ghostel-tmux-integration-test--with-host (host term)
          (cl-letf (((symbol-function 'ghostel-tmux--request-window-rows)
                     #'ignore))
            (ghostel--write-vt term "\eP1000p%session-changed $1 gr\303")
            (ghostel-tmux-integration-test--drain)
            (ghostel--write-vt term "\266\303\237e\r\n")
            (ghostel-tmux-integration-test--drain))
          (should (equal ghostel-tmux--session-name "gr\u00f6\u00dfe"))
          (ghostel--write-vt term "\e\\")
          (ghostel-tmux-integration-test--drain))
      (ghostel-tmux-takeover-mode -1))))

(ert-deftest ghostel-tmux-integration-resize-keeps-repainted-prompt ()
  "A prompt the shell repaints after a pane resize stays on screen."
  :tags '(native)
  (ghostel-tmux-integration-test--with-host (pane term)
    (set-window-buffer (selected-window) pane)
    (let ((prompt "\e]133;A\e\\PROMPT> \e]133;B\e\\"))
      (ghostel--write-vt term (concat "output\r\n" prompt))
      (ghostel--redraw-now pane)
      (ghostel-tmux--resize-pane 20 80)
      ;; tmux resized the pane and the shell repainted its prompt.
      (ghostel--write-vt term (concat "\r\e[K" prompt))
      (ghostel--redraw-now pane)
      (should (string-match-p "PROMPT>" (ghostel--copy-all-text term))))))

(ert-deftest ghostel-tmux-integration-hidden-pane-keeps-repainted-prompt ()
  "A prompt repainted after a layout change survives showing the hidden pane."
  :tags '(native)
  (let ((ctl (generate-new-buffer " *ghostel-tmux-int-ctl*"))
        (prompt "\e]133;A\e\\PROMPT> \e]133;B\e\\")
        pane)
    (unwind-protect
        (save-window-excursion
          (with-current-buffer ctl
            (ghostel-tmux-control-mode))
          (setq pane (ghostel-tmux--get-or-create-pane ctl 1 0 "w"))
          (with-current-buffer pane
            (ghostel--write-vt ghostel--term (concat "output\r\n" prompt)))
          (with-current-buffer ctl
            (ghostel-tmux--on-layout-change 0 "bd5b,100x30,0,0,1"))
          (with-current-buffer pane
            ;; tmux resized the pane and the shell repainted its prompt.
            (ghostel--write-vt ghostel--term (concat "\r\e[K" prompt))
            (set-window-buffer (selected-window) pane)
            (ghostel-tmux--resize-pane 30 100)
            (ghostel--redraw-now pane)
            (should (string-match-p "PROMPT>"
                                    (ghostel--copy-all-text ghostel--term)))))
      (when (buffer-live-p pane) (kill-buffer pane))
      (kill-buffer ctl))))

(ert-deftest ghostel-tmux-integration-pane-outlives-a-deleted-directory ()
  "A pane is made when the directory of its controller is gone."
  :tags '(native)
  (let ((ctl (generate-new-buffer " *ghostel-tmux-int-ctl*"))
        pane)
    (unwind-protect
        (progn
          (with-current-buffer ctl
            (ghostel-tmux-control-mode)
            (setq default-directory "/nonexistent/directory/"))
          (setq pane (ghostel-tmux--get-or-create-pane ctl 1 0 "w"))
          (should (eq pane (gethash 1 (buffer-local-value 'ghostel-tmux--panes ctl))))
          (should (equal (with-current-buffer pane (ghostel-terminal-size))
                         (cons ghostel-tmux-default-cols ghostel-tmux-default-rows))))
      (when (buffer-live-p pane) (kill-buffer pane))
      (kill-buffer ctl))))

(ert-deftest ghostel-tmux-integration-pane-output-sends-no-replies ()
  "A query in a pane's `%output' gets no reply from its terminal; tmux answers."
  :tags '(native)
  (let ((ctl (generate-new-buffer " *ghostel-tmux-int-ctl*"))
        (sent nil)
        pane)
    (unwind-protect
        (progn
          (with-current-buffer ctl
            (ghostel-tmux-control-mode))
          (setq pane (ghostel-tmux--get-or-create-pane ctl 1 0 "w"))
          (with-current-buffer pane
            (setq ghostel-tmux--initialized t
                  ghostel-pty-out-function (lambda (data) (push data sent))))
          (with-current-buffer ctl
            (ghostel-tmux--on-output 1 "\\033[c\\033[6n"))
          (should-not sent)
          ;; The same queries written otherwise are answered.
          (with-current-buffer pane
            (ghostel-write-output "\e[c\e[6n"))
          (should sent))
      (when (buffer-live-p pane) (kill-buffer pane))
      (kill-buffer ctl))))

(ert-deftest ghostel-tmux-integration-sealed-pane-leaves-mouse-to-emacs ()
  "A sealed pane whose program tracked the mouse scrolls and selects again."
  :tags '(native)
  (ghostel-tmux-integration-test--with-host (pane term)
    (dolist (mode '(1000 1002 1003))
      (ghostel--write-vt term (format "\e[?%dh" mode))
      (should (ghostel--mode-enabled term mode))
      (ghostel-tmux--seal-pane-buffer pane)
      (should-not (ghostel--mode-enabled term mode)))))

(ert-deftest ghostel-tmux-integration-sealed-pane-reports-no-focus ()
  "A sealed pane whose program asked for focus reports sends none."
  :tags '(native)
  (ghostel-tmux-integration-test--with-host (pane term)
    (let ((sent nil))
      (setq ghostel-pty-out-function (lambda (data) (push data sent)))
      (ghostel--write-vt term "\e[?1004h")
      (ghostel--focus-event term t)
      (should (equal sent '("\e[I")))
      (ghostel-tmux--seal-pane-buffer pane)
      (should-not (ghostel--focus-event term nil)))))

(provide 'ghostel-tmux-integration-test)

;;; ghostel-tmux-integration-test.el ends here
