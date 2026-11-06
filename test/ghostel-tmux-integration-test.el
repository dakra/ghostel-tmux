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
    display-message*)
      printf '%%begin %d %d 1\\r\\n3.4\\r\\n%%end %d %d 1\\r\\n' $i $i $i $i ;;
    list-windows*)
      printf '%%begin %d %d 1\\r\\n@0\\t%%0\\t1\\tmain\\tbd5b,80x24,0,0,0\\r\\n%%end %d %d 1\\r\\n' $i $i $i $i ;;
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
          ;; The terminal holds at least one of the pieces of text we
          ;; fed in.
          (with-current-buffer created-buf
            (let ((text (ghostel--copy-all-text ghostel--term)))
              (should (or (string-match-p "HISTORY_LINE_ONE" text)
                          (string-match-p "VISIBLE_PROMPT" text)
                          (string-match-p "BANNER_FROM_LIVE" text))))))
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
