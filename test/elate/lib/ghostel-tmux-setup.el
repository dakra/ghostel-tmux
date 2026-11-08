;;; ghostel-tmux-setup.el --- Shared elate startup for ghostel-tmux scenarios -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded through a scenario's `session.load'.  Sets up ghostel from
;; GHOSTEL_DIR and ghostel-tmux from this checkout; a scenario then
;; calls `elate-tmux-init' from `session.eval'.

;;; Code:

(let ((ghostel (or (getenv "GHOSTEL_DIR")
                   (error "GHOSTEL_DIR is not set")))
      (here (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "test/elate/lib/ghostel-setup.el" ghostel))
  (add-to-list 'load-path (expand-file-name "../../../" here)))

;; A stale ghostel-tmux.elc must not shadow an edited source.
(setq load-prefer-newer t)
(require 'ghostel-tmux)

(defvar elate-tmux-dir nil
  "TMUX_TMPDIR of this run's private tmux server.")

(defun elate-tmux-init (name shell native)
  "Give this Emacs a private tmux server named NAME and set up ghostel.
SHELL is `ghostel-shell', NATIVE is `ghostel-use-native-pty'.  A server
left behind by an earlier failed run is killed."
  (setq elate-tmux-dir (format "/tmp/ghostel-tmux-elate-%s" name))
  (make-directory elate-tmux-dir t)
  (setenv "TMUX_TMPDIR" elate-tmux-dir)
  ;; elate hosts this Emacs in a tmux of its own.
  (setenv "TMUX" nil)
  (call-process "tmux" nil nil nil "kill-server")
  ;; Shells start in the sandbox home, wherever elate was started.
  (setq-default default-directory "~/")
  (dolist (b (buffer-list))
    (with-current-buffer b (setq default-directory "~/")))
  (setq ghostel-shell shell
        ghostel-use-native-pty native))

(defun elate-tmux-cleanup ()
  "Kill the private tmux server and remove its directory.
Return non-nil when no server is left."
  ;; Without `elate-tmux-init', tmux would be the server hosting this Emacs.
  (unless elate-tmux-dir
    (error "`elate-tmux-init' has not run"))
  (call-process "tmux" nil nil nil "kill-server")
  ;; Without its directory, tmux would ask the default server.
  (prog1 (not (elate-tmux-server-p))
    (delete-directory elate-tmux-dir t)))

(defun elate-tmux-server-p ()
  "Non-nil when the private tmux server runs."
  (eql 0 (call-process "tmux" nil nil nil "list-sessions")))

(defun elate-tmux-call (&rest args)
  "Run tmux with ARGS outside control mode; return its trimmed output.
A failure signals an error: its message must not pass for an answer."
  (with-temp-buffer
    (let ((status (apply #'call-process "tmux" nil (list t nil) nil args)))
      (unless (eql status 0)
        (error "tmux %s: exit status %s" (car args) status)))
    (string-trim (buffer-string))))

(defun elate-tmux-q (format &optional target)
  "Ask tmux for FORMAT, expanded for TARGET (a pane, window or session)."
  (apply #'elate-tmux-call "display-message" "-p"
         (append (and target (list "-t" target)) (list format))))

(defun elate-tmux-panes ()
  "Return the live pane buffers."
  (seq-filter (lambda (b)
                (and (buffer-local-value 'ghostel-tmux-pane-mode b)
                     (buffer-local-value 'ghostel-tmux--pane-id b)
                     (buffer-local-value 'ghostel-pty-out-function b)))
              (buffer-list)))

(defun elate-tmux-sealed ()
  "Return the sealed pane buffers."
  (seq-filter (lambda (b)
                (and (buffer-local-value 'ghostel-tmux-pane-mode b)
                     (buffer-local-value 'ghostel-tmux--pane-id b)
                     (null (buffer-local-value 'ghostel-pty-out-function b))))
              (buffer-list)))

(defun elate-tmux-pane (&optional id)
  "Return the live pane buffer for tmux pane %ID, or the first live one."
  (if id
      (seq-find (lambda (b) (eql id (buffer-local-value 'ghostel-tmux--pane-id b)))
                (elate-tmux-panes))
    (car (elate-tmux-panes))))

(defun elate-tmux-id (&optional buffer)
  "Return tmux's name (\"%N\") for the pane in BUFFER or the current buffer."
  (format "%%%d" (buffer-local-value 'ghostel-tmux--pane-id
                                     (or buffer (current-buffer)))))

(defun elate-tmux-host ()
  "Return the shell buffer that is inside a control-mode frame."
  (seq-find (lambda (b) (buffer-local-value 'ghostel-tmux--dcs-active b))
            (buffer-list)))

(defun elate-tmux-controllers ()
  "Return the buffers of the `tmux -CC' subprocesses that are alive."
  (seq-filter (lambda (b)
                (and (eq (buffer-local-value 'major-mode b)
                         'ghostel-tmux-control-mode)
                     (process-live-p (get-buffer-process b))))
              (buffer-list)))

(defun elate-tmux-shown ()
  "Return the selected window's buffer when it is an initialized live pane."
  (let ((b (window-buffer (selected-window))))
    (and (memq b (elate-tmux-panes))
         (buffer-local-value 'ghostel-tmux--initialized b)
         b)))

(defun elate-tmux-text (&optional buffer)
  "Return the text of BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun elate-tmux-line-p (regexp &optional buffer)
  "Non-nil when a whole line of BUFFER or the current buffer matches REGEXP.
Trailing spaces are ignored: a backfilled row keeps the ones it had."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (and (re-search-forward (concat "^\\(?:" regexp "\\) *$") nil t) t))))

(defun elate-tmux-prompt-p (&optional buffer)
  "Non-nil when the last line of BUFFER's text looks like a shell prompt.
Nushell prints the time at the right edge of its prompt row."
  (and (string-match-p "[$%>#]\\(?:  +[^\n]*\\)?\\'"
                       (string-trim-right (elate-tmux-text buffer) "[ \n]+"))
       t))

(defun elate-tmux-tabs (&optional buffer)
  "Return the tmux pane names on the tab line of BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (mapcar #'elate-tmux-id (funcall tab-line-tabs-function))))

(defun elate-tmux-session (&optional buffer)
  "Return tmux's name (\"$N\") for the session of the pane in BUFFER."
  (with-current-buffer (buffer-local-value 'ghostel-tmux--controller-buffer
                                           (or buffer (current-buffer)))
    (format "$%d" ghostel-tmux--session-id)))

(defun elate-tmux-sync-p ()
  "Non-nil when tmux's active pane and its size are those of the pane shown."
  (when-let* ((b (elate-tmux-shown)))
    (with-current-buffer b
      (equal (elate-tmux-q "#{pane_id} #{pane_width}x#{pane_height}"
                           (elate-tmux-session))
             (format "%s %dx%d" (elate-tmux-id)
                     ghostel--term-cols ghostel--term-rows)))))

(defun elate-tmux-attached-p ()
  "Non-nil while a control-mode client of this Emacs is attached."
  (and (or (elate-tmux-host) (elate-tmux-controllers)) t))

(defun elate-tmux-wrapper (ext)
  "Return the path of the shell wrapper with extension EXT."
  (expand-file-name (concat "shell/ghostel-tmux." ext)
                    (file-name-directory (locate-library "ghostel-tmux"))))

(defun elate-tmux-asking-p (prompt)
  "Non-nil when the minibuffer is open with PROMPT."
  (when-let* ((w (active-minibuffer-window)))
    (with-current-buffer (window-buffer w)
      (equal (minibuffer-prompt) prompt))))

(defun elate-tmux-rows-p (start)
  "Non-nil when the pane shows the numbers from START on all rows but the last."
  (string-prefix-p
   (mapconcat #'number-to-string
              (number-sequence start (+ start ghostel--term-rows -2)) "\n")
   (elate-tmux-text)))

(defun elate-tmux-client-pid ()
  "Return the process id of the one attached tmux client."
  (let ((pids (split-string (elate-tmux-call "list-clients" "-F" "#{client_pid}"))))
    (unless (= 1 (length pids))
      (error "Expected one tmux client, got %S" pids))
    (string-to-number (car pids))))

(defun elate-tmux-parent-pid (pid)
  "Return the process id of the parent of PID."
  (with-temp-buffer
    (call-process "ps" nil t nil "-o" "ppid=" "-p" (number-to-string pid))
    (string-to-number (buffer-string))))

(defun elate-tmux-kill (pid)
  "Send SIGKILL to the process PID.
Pid 0 would be this Emacs's whole process group."
  (unless (and (integerp pid) (> pid 1))
    (error "Not a process id: %S" pid))
  (signal-process pid 'SIGKILL))

(defun elate-tmux-reset-if-attached ()
  "Run `ghostel-tmux-reset' when a shell is still in control mode."
  (when-let* ((host (elate-tmux-host)))
    (with-current-buffer host (ghostel-tmux-reset))
    t))

(defun elate-tmux-split-windows ()
  "Return the windows of the split of the selected window's tmux window."
  (when-let* ((tile (window-parameter (selected-window) 'ghostel-tmux-tile)))
    (ghostel-tmux--tile-windows (nth 0 tile) (nth 1 tile))))

(defun elate-tmux-split-p (n)
  "Non-nil when the selected window's split has N windows, each the size of its pane.
Each window shows its pane, at the columns and rows tmux gives the pane."
  (let ((windows (elate-tmux-split-windows)))
    (and (= n (length windows))
         (seq-every-p
          (lambda (w)
            (let* ((pid (nth 2 (window-parameter w 'ghostel-tmux-tile)))
                   (buf (elate-tmux-pane pid)))
              (and (eq (window-buffer w) buf)
                   (equal (elate-tmux-q "#{pane_width}x#{pane_height}" (format "%%%d" pid))
                          (format "%dx%d" (window-max-chars-per-line w)
                                  (floor (with-selected-window w (window-screen-lines)))))
                   (with-current-buffer buf
                     (equal (cons ghostel--term-cols ghostel--term-rows)
                            (cons (window-max-chars-per-line w)
                                  (floor (with-selected-window w
                                           (window-screen-lines)))))))))
          windows))))

(defun elate-tmux-active-p ()
  "Non-nil when tmux's active pane is the one the selected window shows."
  (when-let* ((b (elate-tmux-shown)))
    (equal (elate-tmux-q "#{pane_id}" (elate-tmux-session b)) (elate-tmux-id b))))

(defun elate-tmux-no-split-p ()
  "Non-nil when no window is part of a split."
  (not (seq-some (lambda (w) (window-parameter w 'ghostel-tmux-tile))
                 (window-list-1 nil 'no-mini t))))

(provide 'ghostel-tmux-setup)
;;; ghostel-tmux-setup.el ends here
