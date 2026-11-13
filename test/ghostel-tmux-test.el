;;; ghostel-tmux-test.el --- Tests for ghostel-tmux -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure-Elisp tests for the tmux protocol layer (parser, octal decoder,
;; layout parser, hex encoder, command queue).  DCS framing is
;; libghostty's parser, covered by the native tests in
;; ghostel-tmux-integration-test.el; the end-to-end tmux integration
;; is covered there too and verified live.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'files-x)
(require 'ghostel-tmux)

(defvar ghostel-control-mode-function)

;; The unit tests run without the native module.
(advice-add 'ghostel-ensure-module :override #'ignore)
(unless (fboundp 'ghostel--new)
  (defalias 'ghostel--new #'ignore))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defmacro ghostel-tmux-test--with-controller (var &rest body)
  "Bind VAR to a fresh controller buffer, run BODY inside it."
  (declare (indent 1))
  `(let ((,var (generate-new-buffer " *ghostel-tmux-test-ctl*")))
     (unwind-protect
         (with-current-buffer ,var
           (ghostel-tmux-control-mode)
           ,@body)
       (kill-buffer ,var))))

(defun ghostel-tmux-test--feed (controller string)
  "Feed STRING into CONTROLLER's parser."
  (with-current-buffer controller
    (ghostel-tmux--feed-bytes string)))

;; ---------------------------------------------------------------------------
;; Failing lines
;; ---------------------------------------------------------------------------

(ert-deftest ghostel-tmux-test-feed-survives-failing-line ()
  "A line whose handler signals is consumed; later lines and chunks are handled."
  (ghostel-tmux-test--with-controller ctl
    (let ((seen nil)
          (reply nil))
      (cl-letf (((symbol-function 'ghostel-tmux--on-output)
                 (lambda (_pid data) (push data seen)))
                ((symbol-function 'ghostel-tmux--controller-live-p)
                 (lambda () t))
                ((symbol-function 'ghostel-tmux--controller-send) #'ignore))
        (ghostel-tmux--queue-command "x" (lambda (out) (setq reply out)))
        (ghostel-tmux-test--feed
         ctl (concat "\eP1000p%output %0 a\r\n"
                     "%layout-change @0 ffff,bogus\r\n"
                     "%output %0 b\r\n"))
        (should (equal ghostel-tmux--line-buffer ""))
        (ghostel-tmux-test--feed
         ctl "%output %0 c\r\n%begin 1 2 1\r\nok\r\n%end 1 2 1\r\n")
        (should (equal (reverse seen) '("a" "b" "c")))
        (should (equal reply "ok"))
        (should (equal ghostel-tmux--line-buffer ""))))))

(ert-deftest ghostel-tmux-test-feed-takes-doubled-cr ()
  "A terminator or `%output' with a doubled CR is still what it is.
tmux's tty turns LF into CRLF, and on macOS a non-blocking write that
fills its output queue between the two repeats the CR."
  (ghostel-tmux-test--with-controller ctl
    (let ((seen nil)
          (reply nil))
      (cl-letf (((symbol-function 'ghostel-tmux--on-output)
                 (lambda (_pid data) (push data seen)))
                ((symbol-function 'ghostel-tmux--controller-live-p)
                 (lambda () t))
                ((symbol-function 'ghostel-tmux--controller-send) #'ignore))
        (ghostel-tmux--queue-command "x" (lambda (out) (setq reply out)))
        (ghostel-tmux-test--feed
         ctl (concat "\eP1000p%output %0 a\r\r\n"
                     "%begin 1 2 1\r\r\nok\r\r\n%end 1 2 1\r\r\n"))
        (should (equal seen '("a")))
        (should (equal reply "ok"))
        (should-not ghostel-tmux--command-in-flight)))))

(ert-deftest ghostel-tmux-test-feed-keeps-position-on-nonlocal-exit ()
  "A non-error exit from a handler still consumes the lines handled so far."
  (ghostel-tmux-test--with-controller ctl
    (let ((seen nil))
      (cl-letf (((symbol-function 'ghostel-tmux--on-output)
                 (lambda (_pid data)
                   (push data seen)
                   (when (equal data "b") (throw 'stop nil)))))
        (catch 'stop
          (ghostel-tmux-test--feed
           ctl "\eP1000p%output %0 a\r\n%output %0 b\r\n%output %0 c\r\n"))
        (should (equal ghostel-tmux--line-buffer "%output %0 c\r\n"))
        (ghostel-tmux-test--feed ctl "")
        (should (equal (reverse seen) '("a" "b" "c")))))))

;; `kill-buffer' leaves `other-buffer' current; `set-buffer' pins which one.
(ert-deftest ghostel-tmux-test-feed-spares-next-buffer-when-killed ()
  "A handler killing the feed buffer leaves the next current buffer's state alone."
  (ghostel-tmux-test--with-controller other
    (setq ghostel-tmux--line-buffer "%output %1 unconsumed\r\n")
    (let ((lines nil))
      (ghostel-tmux-test--with-controller ctl
        (cl-letf (((symbol-function 'ghostel-tmux--handle-line)
                   (lambda (line)
                     (push line lines)
                     (kill-buffer ctl)
                     (set-buffer other))))
          (ghostel-tmux-test--feed
           ctl "\eP1000p%output %0 a\r\n%output %0 b\r\n")))
      (should (equal lines '("%output %0 a"))))
    (should (equal ghostel-tmux--line-buffer "%output %1 unconsumed\r\n"))))

;; ---------------------------------------------------------------------------
;; Octal escape decoder
;; ---------------------------------------------------------------------------

(ert-deftest ghostel-tmux-test-decode-no-escapes ()
  (should (equal (ghostel-tmux--decode-output "hello world")
                 "hello world")))

(ert-deftest ghostel-tmux-test-decode-esc ()
  (should (equal (ghostel-tmux--decode-output "\\033[1mFOO\\033[0m")
                 "\x1b[1mFOO\x1b[0m")))

(ert-deftest ghostel-tmux-test-decode-crlf ()
  (should (equal (ghostel-tmux--decode-output "line1\\015\\012line2")
                 "line1\r\nline2")))

(ert-deftest ghostel-tmux-test-decode-bel ()
  (should (equal (ghostel-tmux--decode-output "\\007")
                 "\x07")))

(ert-deftest ghostel-tmux-test-decode-mixed ()
  (should (equal (ghostel-tmux--decode-output
                  "\\033[?2004hroot@host:/# \\015\\012")
                 "\x1b[?2004hroot@host:/# \r\n")))

(ert-deftest ghostel-tmux-test-decode-non-octal-backslash ()
  ;; \X (not three octal digits) is left verbatim
  (should (equal (ghostel-tmux--decode-output "a\\nb")
                 "a\\nb")))

(ert-deftest ghostel-tmux-test-decode-empty ()
  (should (equal (ghostel-tmux--decode-output "") "")))

(ert-deftest ghostel-tmux-test-decode-high-octal-single-byte ()
  ;; \310 must decode to the single raw byte 0xC8, not the two-byte
  ;; UTF-8 form of U+00C8 that `string' + binary encoding produces.
  (let ((decoded (ghostel-tmux--decode-output "\\310")))
    (should-not (multibyte-string-p decoded))
    (should (equal (append decoded nil) '(#xC8)))))

(ert-deftest ghostel-tmux-test-decode-high-octal-mixed-with-utf8 ()
  ;; Raw multibyte pane content mixed with a high octal escape: the
  ;; UTF-8 text keeps its bytes and the escape stays a single byte.
  (let ((decoded (ghostel-tmux--decode-output "×\\310")))
    (should-not (multibyte-string-p decoded))
    (should (equal (append decoded nil) '(#xC3 #x97 #xC8)))))

(ert-deftest ghostel-tmux-test-decode-escaped-backslash ()
  ;; capture-pane -C writes a backslash as `\\'; what follows it is text.
  (should (equal (ghostel-tmux--decode-output "a\\\\b\\\\033")
                 "a\\b\\033")))

(ert-deftest ghostel-tmux-test-decode-backslash-is-not-decoded-again ()
  (should (equal (ghostel-tmux--decode-output "\\134033") "\\033")))

(ert-deftest ghostel-tmux-test-decode-incomplete-escape ()
  (should (equal (ghostel-tmux--decode-output "a\\01") "a\\01"))
  (should (equal (ghostel-tmux--decode-output "a\\") "a\\")))

;; ---------------------------------------------------------------------------
;; Layout parser
;; ---------------------------------------------------------------------------

(ert-deftest ghostel-tmux-test-layout-single-leaf ()
  (should (equal (ghostel-tmux--parse-layout "bd5b,80x24,0,0,0")
                 '((0 80 24 0 0)))))

(ert-deftest ghostel-tmux-test-layout-h-split ()
  (should (equal (ghostel-tmux--parse-layout
                  "f8f9,80x24,0,0{40x24,0,0,1,40x24,40,0,2}")
                 '((1 40 24 0 0) (2 40 24 40 0)))))

(ert-deftest ghostel-tmux-test-layout-v-split ()
  (should (equal (ghostel-tmux--parse-layout
                  "1234,80x24,0,0[80x12,0,0,3,80x12,0,12,4]")
                 '((3 80 12 0 0) (4 80 12 0 12)))))

(ert-deftest ghostel-tmux-test-layout-nested ()
  ;; Outer h-split: left leaf, right v-split.
  (should (equal (ghostel-tmux--parse-layout
                  "abcd,80x24,0,0{40x24,0,0,1,40x24,40,0[40x12,40,0,2,40x12,40,12,3]}")
                 '((1 40 24 0 0) (2 40 12 40 0) (3 40 12 40 12)))))

(ert-deftest ghostel-tmux-test-layout-empty-returns-nil ()
  (should (null (ghostel-tmux--parse-layout nil)))
  (should (null (ghostel-tmux--parse-layout "")))
  ;; A window of floating panes only.
  (should (null (ghostel-tmux--parse-layout "0000,")))
  (should (null (ghostel-tmux--parse-layout-tree "0000,"))))

(ert-deftest ghostel-tmux-test-layout-tree-leaf ()
  (should (equal (ghostel-tmux--parse-layout-tree "bd5b,80x24,0,0,7")
                 '(pane 80 24 0 0 7))))

(ert-deftest ghostel-tmux-test-layout-tree-nested ()
  "Side by side is `h', stacked is `v', children in tmux's order."
  (should (equal (ghostel-tmux--parse-layout-tree
                  "abcd,80x24,0,0{40x24,0,0,1,39x24,41,0[39x12,41,0,2,39x11,41,13,3]}")
                 '(h 80 24 0 0
                     ((pane 40 24 0 0 1)
                      (v 39 24 41 0
                         ((pane 39 12 41 0 2)
                          (pane 39 11 41 13 3))))))))

(ert-deftest ghostel-tmux-test-layout-tree-three-way ()
  (should (equal (ghostel-tmux--parse-layout-tree
                  "abcd,80x24,0,0[80x8,0,0,1,80x7,0,9,2,80x7,0,17,3]")
                 '(v 80 24 0 0
                     ((pane 80 8 0 0 1)
                      (pane 80 7 0 9 2)
                      (pane 80 7 0 17 3))))))

(ert-deftest ghostel-tmux-test-layout-tree-row-in-column ()
  (should (equal (ghostel-tmux--parse-layout-tree
                  "abcd,80x24,0,0[80x12,0,0{40x12,0,0,1,39x12,41,0,2},80x11,0,13,3]")
                 '(v 80 24 0 0
                     ((h 80 12 0 0
                         ((pane 40 12 0 0 1)
                          (pane 39 12 41 0 2)))
                      (pane 80 11 0 13 3))))))

(ert-deftest ghostel-tmux-test-layout-tree-malformed-signals ()
  (should-error (ghostel-tmux--parse-layout-tree "abcd,80x24,0,0{40x24,0,0,1"))
  (should-error (ghostel-tmux--parse-layout-tree
                 "abcd,80x24,0,0{40x24,0,0,1,39x24,41,0,2]"))
  (should-error (ghostel-tmux--parse-layout-tree "abcd,80x24,0,0;1")))

;; ---------------------------------------------------------------------------
;; Hex encoding for send-keys -H
;; ---------------------------------------------------------------------------

(ert-deftest ghostel-tmux-test-hex-crlf ()
  (should (equal (ghostel-tmux--bytes-to-hex "\r\n") "0d 0a")))

(ert-deftest ghostel-tmux-test-hex-printable ()
  (should (equal (ghostel-tmux--bytes-to-hex "abc") "61 62 63")))

(ert-deftest ghostel-tmux-test-hex-esc-sequence ()
  (should (equal (ghostel-tmux--bytes-to-hex "\e[A")
                 "1b 5b 41")))

(ert-deftest ghostel-tmux-test-hex-empty ()
  (should (equal (ghostel-tmux--bytes-to-hex "") "")))

;; ---------------------------------------------------------------------------
;; Wire-format parser
;; ---------------------------------------------------------------------------

(ert-deftest ghostel-tmux-test-parser-strips-dcs-preamble ()
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--feed ctl "\eP1000p%begin 1 2 0\r\n%end 1 2 0\r\n")
    (should (eq ghostel-tmux--parser-state 'idle))
    (should (string-empty-p ghostel-tmux--line-buffer))))

(ert-deftest ghostel-tmux-test-parser-session-changed ()
  (ghostel-tmux-test--with-controller ctl
    ;; Skip preamble/idle bootstrap by switching state directly.
    (setq ghostel-tmux--parser-state 'idle)
    (ghostel-tmux-test--feed ctl "%session-changed $42 work\r\n")
    (should (= ghostel-tmux--session-id 42))
    (should (equal ghostel-tmux--session-name "work"))))

(ert-deftest ghostel-tmux-test-parser-output-octal-decoded ()
  (let (called-with-pid called-with-data)
    (cl-letf (((symbol-function 'ghostel-tmux--on-output)
               (lambda (pid data)
                 (setq called-with-pid pid called-with-data data))))
      (ghostel-tmux-test--with-controller ctl
        (setq ghostel-tmux--parser-state 'idle)
        (ghostel-tmux-test--feed ctl
                                 "%output %5 hi\\015\\012\r\n")
        (should (= called-with-pid 5))
        (should (equal called-with-data "hi\\015\\012"))))))

(ert-deftest ghostel-tmux-test-parser-block-end-runs-cont ()
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--parser-state 'idle)
    (let ((received nil))
      (setq ghostel-tmux--command-in-flight
            (cons "display-message" (lambda (out) (setq received out))))
      (ghostel-tmux-test--feed
       ctl "%begin 1 7 1\r\nhello world\r\n%end 1 7 1\r\n")
      (should (equal received "hello world"))
      (should (eq ghostel-tmux--parser-state 'idle))
      (should (null ghostel-tmux--command-in-flight)))))

(ert-deftest ghostel-tmux-test-parser-block-error-runs-cont-with-nil ()
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--parser-state 'idle)
    (let ((called nil) (received 'unset))
      (setq ghostel-tmux--command-in-flight
            (cons "bad" (lambda (out)
                          (setq called t received out))))
      (ghostel-tmux-test--feed
       ctl "%begin 1 7 1\r\nbad cmd\r\n%error 1 7 1\r\n")
      (should called)
      (should (null received)))))

(ert-deftest ghostel-tmux-test-parser-block-payload-with-percent-end ()
  ;; A line in the block that *looks* like a terminator but has wrong tag
  ;; count must be treated as payload.
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--parser-state 'idle)
    (let ((received nil))
      (setq ghostel-tmux--command-in-flight
            (cons "x" (lambda (out) (setq received out))))
      (ghostel-tmux-test--feed
       ctl
       (concat
        "%begin 1 9 1\r\n"
        "%end notatag\r\n"          ; non-numeric — payload
        "real line\r\n"
        "%end 1 9 1\r\n"))
      (should (equal received "%end notatag\nreal line")))))

(ert-deftest ghostel-tmux-test-parser-block-forged-end-is-payload ()
  ;; A terminator-shaped line whose TIME/CMD fields don't match the
  ;; opening %begin tag is block payload (e.g. a captured pane that was
  ;; showing a control-mode transcript), not the block's end.
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--parser-state 'idle)
    (let ((received nil))
      (setq ghostel-tmux--command-in-flight
            (cons "capture-pane" (lambda (out) (setq received out))))
      (ghostel-tmux-test--feed
       ctl
       (concat
        "%begin 1622 17 1\r\n"
        "%end 99 99 0\r\n"           ; forged — wrong TIME/CMD
        "real line\r\n"
        "%end 1622 17 1\r\n"))
      (should (equal received "%end 99 99 0\nreal line"))
      (should (eq ghostel-tmux--parser-state 'idle)))))

(ert-deftest ghostel-tmux-test-parser-multi-chunk ()
  ;; Notification arrives split across two filter calls.
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--parser-state 'idle)
    (ghostel-tmux-test--feed ctl "%session-cha")
    (should (eq ghostel-tmux--parser-state 'idle))
    (should (null ghostel-tmux--session-id))
    (ghostel-tmux-test--feed ctl "nged $7 main\r\n")
    (should (= ghostel-tmux--session-id 7))
    (should (equal ghostel-tmux--session-name "main"))))

(ert-deftest ghostel-tmux-test-parser-ignores-bare-lines ()
  ;; tmux delivers async `run-shell' output as bare lines outside any
  ;; %begin/%end block; they must be dropped, not wedge the parser.
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--parser-state 'idle)
    (ghostel-tmux-test--feed ctl "run-shell output line\r\n")
    (should (eq ghostel-tmux--parser-state 'idle))
    ;; The stream keeps parsing afterwards.
    (ghostel-tmux-test--feed ctl "%session-changed $7 main\r\n")
    (should (= ghostel-tmux--session-id 7))))

(ert-deftest ghostel-tmux-test-parser-junk-before-first-notification ()
  ;; With the DCS preamble missing (wrapper script, tmux warning line),
  ;; junk preceding the first `%' line must not prevent the parser from
  ;; picking up the protocol.
  (ghostel-tmux-test--with-controller ctl
    (should (eq ghostel-tmux--parser-state 'preamble))
    (ghostel-tmux-test--feed
     ctl "some config warning\r\n%session-changed $3 demo\r\n")
    (should (eq ghostel-tmux--parser-state 'idle))
    (should (= ghostel-tmux--session-id 3))))

;; ---------------------------------------------------------------------------
;; Command queue
;; ---------------------------------------------------------------------------

(ert-deftest ghostel-tmux-test-command-queue-fifo ()
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (cl-letf (((symbol-function 'process-send-string)
                 (lambda (_p s) (push s sent)))
                ((symbol-function 'process-live-p) (lambda (_) t)))
        (setq ghostel-tmux--controller-process 'fake)
        (ghostel-tmux--queue-command "first" #'ignore)
        (ghostel-tmux--queue-command "second" #'ignore)
        ;; Only the first command should have been sent.
        (should (equal (nreverse sent) '("first\n")))
        (should (equal (caar ghostel-tmux--command-queue) "second"))
        ;; Simulate %end terminating the first command.
        (setq ghostel-tmux--command-in-flight nil)
        (ghostel-tmux--pump-command-queue)
        (should (member "second\n" sent))))))

(ert-deftest ghostel-tmux-test-send-bytes-waits-behind-in-flight-command ()
  ;; Keystrokes must not overtake a command awaiting its reply block:
  ;; tmux pairs reply blocks to command lines in wire order, so a raw
  ;; send while another command is in flight shifts every subsequent
  ;; pairing by one.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (with-current-buffer ctl
        (setq ghostel-tmux--controller-process 'fake-proc
              ghostel-tmux--command-in-flight (cons "list-windows" #'ignore)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (push s sent))))
        (ghostel-tmux--send-bytes-from-pane ctl 0 "x"))
      (should (null sent))
      (with-current-buffer ctl
        (should (= 1 (length ghostel-tmux--command-queue)))
        (should (string-prefix-p "send-keys -t %0 -H "
                                 (caar ghostel-tmux--command-queue)))))))

(ert-deftest ghostel-tmux-test-send-bytes-joins-queued-keys ()
  ;; Keys typed while a command is in flight join the `send-keys' of
  ;; the same pane at the end of the queue.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (with-current-buffer ctl
        (setq ghostel-tmux--controller-process 'fake-proc
              ghostel-tmux--command-in-flight (cons "list-windows" #'ignore)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (push s sent))))
        (ghostel-tmux--send-bytes-from-pane ctl 1 "ab")
        (ghostel-tmux--send-bytes-from-pane ctl 1 "c")
        (with-current-buffer ctl
          (should (equal (mapcar #'car ghostel-tmux--command-queue)
                         '("send-keys -t %1 -H 61 62 63"))))
        ;; Another pane's keys, then any other command, end the run.
        (ghostel-tmux--send-bytes-from-pane ctl 10 "d")
        (ghostel-tmux--send-bytes-from-pane ctl 1 "e")
        (with-current-buffer ctl
          (ghostel-tmux--queue-command "select-pane -t %1"))
        (ghostel-tmux--send-bytes-from-pane ctl 1 "f")
        (with-current-buffer ctl
          (should (equal (mapcar #'car ghostel-tmux--command-queue)
                         '("send-keys -t %1 -H 61 62 63"
                           "send-keys -t %10 -H 64"
                           "send-keys -t %1 -H 65"
                           "select-pane -t %1"
                           "send-keys -t %1 -H 66")))))
      (should (null sent)))))

(ert-deftest ghostel-tmux-test-send-bytes-joins-up-to-chunk ()
  ;; A joined command holds at most `--send-keys-chunk-bytes' bytes.
  (ghostel-tmux-test--with-controller ctl
    (with-current-buffer ctl
      (setq ghostel-tmux--controller-process 'fake-proc
            ghostel-tmux--command-in-flight (cons "list-windows" #'ignore)))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-send-string) #'ignore))
      (ghostel-tmux--send-bytes-from-pane
       ctl 0 (make-string (1- ghostel-tmux--send-keys-chunk-bytes) ?a))
      (ghostel-tmux--send-bytes-from-pane ctl 0 "b")
      (ghostel-tmux--send-bytes-from-pane ctl 0 "c")
      (with-current-buffer ctl
        (should (= 2 (length ghostel-tmux--command-queue)))
        (should (string-suffix-p " 61 62" (caar ghostel-tmux--command-queue)))
        (should (equal (car (nth 1 ghostel-tmux--command-queue))
                       "send-keys -t %0 -H 63"))))))

(ert-deftest ghostel-tmux-test-send-bytes-joined-keys-on-the-wire ()
  ;; The joined command goes out as one line once the reply to the
  ;; command in flight arrives.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (with-current-buffer ctl
        (setq ghostel-tmux--controller-process 'fake-proc
              ghostel-tmux--parser-state 'idle
              ghostel-tmux--command-in-flight (cons "list-windows" #'ignore)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (push s sent))))
        (ghostel-tmux--send-bytes-from-pane ctl 0 "a")
        (ghostel-tmux--send-bytes-from-pane ctl 0 "b")
        (ghostel-tmux-test--feed ctl "%begin 1 1 1\r\n%end 1 1 1\r\n")
        (with-current-buffer ctl
          (ghostel-tmux--pump-command-queue)
          (should-not ghostel-tmux--command-queue)))
      (should (equal sent '("send-keys -t %0 -H 61 62\n"))))))

(ert-deftest ghostel-tmux-test-send-bytes-not-joined-after-esc ()
  ;; A key after a lone ESC goes in a command of its own, as the
  ;; program would read the two together as Meta and the key.
  (ghostel-tmux-test--with-controller ctl
    (with-current-buffer ctl
      (setq ghostel-tmux--controller-process 'fake-proc
            ghostel-tmux--command-in-flight (cons "list-windows" #'ignore)))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-send-string) #'ignore))
      (ghostel-tmux--send-bytes-from-pane ctl 0 "\e")
      (ghostel-tmux--send-bytes-from-pane ctl 0 "k")
      (ghostel-tmux--send-bytes-from-pane ctl 0 "a")
      (ghostel-tmux--send-bytes-from-pane ctl 0 "\e")
      (ghostel-tmux--send-bytes-from-pane ctl 0 "\ek"))
    (with-current-buffer ctl
      (should (equal (mapcar #'car ghostel-tmux--command-queue)
                     '("send-keys -t %0 -H 1b"
                       "send-keys -t %0 -H 6b 61 1b"
                       "send-keys -t %0 -H 1b 6b"))))))

(ert-deftest ghostel-tmux-test-send-bytes-not-joined-onto-continuation ()
  ;; A queued command with a continuation, such as a typed `send-keys',
  ;; is not joined.
  (ghostel-tmux-test--with-controller ctl
    (with-current-buffer ctl
      (setq ghostel-tmux--controller-process 'fake-proc
            ghostel-tmux--command-in-flight (cons "list-windows" #'ignore)
            ghostel-tmux--command-queue
            (list (cons "send-keys -t %0 -H 61" #'ignore))))
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-send-string) #'ignore))
      (ghostel-tmux--send-bytes-from-pane ctl 0 "b"))
    (with-current-buffer ctl
      (should (equal (mapcar #'car ghostel-tmux--command-queue)
                     '("send-keys -t %0 -H 61"
                       "send-keys -t %0 -H 62"))))))

(ert-deftest ghostel-tmux-test-pump-defers-inside-feed ()
  ;; Sends triggered from inside the feed (a continuation queueing a
  ;; follow-up command) must be deferred to a timer: a blocking process
  ;; write can re-enter process filters and deliver protocol bytes out
  ;; of order.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil) (timers nil))
      (with-current-buffer ctl
        (setq ghostel-tmux--parser-state 'idle
              ghostel-tmux--controller-process 'fake-proc
              ghostel-tmux--command-in-flight
              (cons "list-windows"
                    (lambda (_out)
                      (ghostel-tmux--queue-command "capture-pane")))))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (push s sent)))
                ((symbol-function 'run-at-time)
                 (lambda (_time _repeat fn &rest args)
                   (when (eq fn #'ghostel-tmux--pump-deferred)
                     (push (cons fn args) timers)))))
        (ghostel-tmux-test--feed ctl "%begin 1 1 1\r\n%end 1 1 1\r\n")
        ;; Nothing sent synchronously; a deferred pump was scheduled.
        (should (null sent))
        (should (= 1 (length timers)))
        ;; Firing the timer outside the feed performs the send.
        (pcase-let ((`(,fn . ,args) (car timers)))
          (apply fn args))
        (should (equal sent '("capture-pane\n")))))))

;; ---------------------------------------------------------------------------
;; Buffer naming
;; ---------------------------------------------------------------------------

(ert-deftest ghostel-tmux-test-default-buffer-name ()
  (should (equal (ghostel-tmux--default-buffer-name "work" "shell" 3)
                 "*ghostel-tmux:work:shell%3*"))
  (should (equal (ghostel-tmux--default-buffer-name "work" nil 3)
                 "*ghostel-tmux:work:<unnamed>%3*"))
  (should (equal (ghostel-tmux--default-buffer-name nil "shell" 3)
                 "*ghostel-tmux:session:shell%3*")))

;; ---------------------------------------------------------------------------
;; DCS callback dispatch (framing is libghostty's parser, tested natively
;; in ghostel-tmux-integration-test.el, so these tests focus on the elisp side: do
;; the callbacks correctly enter/exit control mode and route body bytes
;; to the parser?)
;; ---------------------------------------------------------------------------

(defmacro ghostel-tmux-test--with-host (var &rest body)
  "Bind VAR to a fresh ghostel-mode-like host buffer; run BODY inside."
  (declare (indent 1))
  `(let ((,var (generate-new-buffer " *ghostel-tmux-test-host*")))
     (unwind-protect
         (with-current-buffer ,var
           ;; Don't run real ghostel-mode (no native module needed for
           ;; these tests); just behave like a non-pane non-controller
           ;; buffer.
           (setq-local ghostel-tmux--dcs-active nil)
           ,@body)
       (when (buffer-live-p ,var)
         (when (and (boundp 'ghostel-tmux--panes) ghostel-tmux--panes)
           (maphash (lambda (_pid pb)
                      (when (buffer-live-p pb) (kill-buffer pb)))
                    ghostel-tmux--panes))
         (kill-buffer ,var)))))

(ert-deftest ghostel-tmux-test-dcs-enter-callback ()
  ;; In a normal ghostel buffer an opening frame should run
  ;; `--enter-control-mode' and set the buffer-local active flag.
  (ghostel-tmux-test--with-host host
    (let ((entered-buf nil))
      (cl-letf (((symbol-function 'ghostel-tmux--enter-control-mode)
                 (lambda (buf) (setq entered-buf buf))))
        (ghostel-tmux--dcs-enter))
      (should (eq entered-buf host))
      (should ghostel-tmux--dcs-active))))

(ert-deftest ghostel-tmux-test-dcs-enter-skips-pane-buffer ()
  ;; Pane buffers occasionally have `%output' content that looks like
  ;; a DCS — the callback must not enter control mode in that context.
  (let ((pane (generate-new-buffer "*ghostel-tmux:p%0*")))
    (unwind-protect
        (let ((entered nil))
          (with-current-buffer pane
            (ghostel-tmux-pane-mode 1)
            (cl-letf (((symbol-function 'ghostel-tmux--enter-control-mode)
                       (lambda (_) (setq entered t))))
              (ghostel-tmux--dcs-enter))
            (should-not entered)
            (should-not ghostel-tmux--dcs-active)))
      (kill-buffer pane))))

(ert-deftest ghostel-tmux-test-dcs-enter-idempotent ()
  ;; A spurious second enter (e.g. parser confusion) shouldn't re-run
  ;; setup.  The flag is the gate.
  (ghostel-tmux-test--with-host host
    (setq-local ghostel-tmux--dcs-active t)
    (let ((calls 0))
      (cl-letf (((symbol-function 'ghostel-tmux--enter-control-mode)
                 (lambda (_) (cl-incf calls))))
        (ghostel-tmux--dcs-enter))
      (should (= calls 0)))))

(ert-deftest ghostel-tmux-test-dcs-enter-returns-acceptance ()
  ;; `ghostel-tmux--control' uses the return value to tell the user when
  ;; a control-mode frame was declined (output hidden until ST).
  (ghostel-tmux-test--with-host host
    (cl-letf (((symbol-function 'ghostel-tmux--enter-control-mode) #'ignore))
      (should (ghostel-tmux--dcs-enter))
      ;; Already active: still accepted, no re-entry.
      (should (ghostel-tmux--dcs-enter))
      (setq ghostel-tmux--dcs-active nil)
      (ghostel-tmux-pane-mode 1)
      (should-not (ghostel-tmux--dcs-enter)))))

(ert-deftest ghostel-tmux-test-control-dispatches-events ()
  (ghostel-tmux-test--with-host host
    (let ((ghostel-tmux-takeover-mode t)
          calls)
      (cl-letf (((symbol-function 'ghostel-tmux--dcs-enter)
                 (lambda () (push 'enter calls) t))
                ((symbol-function 'ghostel-tmux--dcs-data)
                 (lambda (data) (push data calls)))
                ((symbol-function 'ghostel-tmux--dcs-exit)
                 (lambda () (push 'exit calls))))
        (ghostel-tmux--control 'enter nil)
        (ghostel-tmux--control 'data "%begin")
        (ghostel-tmux--control 'exit nil))
      (should (equal '(enter "%begin" exit) (nreverse calls))))))

(ert-deftest ghostel-tmux-test-new-pane-outlives-a-deleted-directory ()
  (ghostel-tmux-test--with-controller ctl
    (let (buf)
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-make-terminal)
                     (lambda (name &rest _) (generate-new-buffer name))))
            (setq default-directory "/nonexistent/directory/")
            (setq buf (ghostel-tmux--get-or-create-pane ctl 5 0))
            (should (equal (expand-file-name "~/")
                           (buffer-local-value 'default-directory buf)))
            (should (eq buf (gethash 5 ghostel-tmux--panes))))
        (setq default-directory (expand-file-name "~/"))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest ghostel-tmux-test-new-pane-keeps-a-local-directory ()
  (ghostel-tmux-test--with-controller ctl
    (let ((kept temporary-file-directory)
          buf)
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-make-terminal)
                     (lambda (name &rest _) (generate-new-buffer name))))
            (setq default-directory kept)
            (setq buf (ghostel-tmux--get-or-create-pane ctl 5 0))
            (should (equal kept (buffer-local-value 'default-directory buf))))
        (setq default-directory (expand-file-name "~/"))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest ghostel-tmux-test-new-pane-keeps-a-remote-directory ()
  (ghostel-tmux-test--with-controller ctl
    (let (buf)
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-make-terminal)
                     (lambda (name &rest _) (generate-new-buffer name)))
                    ((symbol-function 'file-directory-p)
                     (lambda (_) (error "Asked the remote host")))
                    ((symbol-function 'file-remote-p) #'always))
            (setq default-directory "/ssh:host:/tmp/")
            (setq buf (ghostel-tmux--get-or-create-pane ctl 5 0))
            (should (equal "/ssh:host:/tmp/"
                           (buffer-local-value 'default-directory buf))))
        (setq default-directory (expand-file-name "~/"))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest ghostel-tmux-test-failed-pane-leaves-nothing ()
  (ghostel-tmux-test--with-controller ctl
    (cl-letf (((symbol-function 'ghostel-make-terminal)
               (lambda (&rest _) (error "No terminal"))))
      (should-error (ghostel-tmux--get-or-create-pane ctl 5 0))
      (should-not (gethash 5 ghostel-tmux--panes)))))

(ert-deftest ghostel-tmux-test-attach-targets-the-exact-session ()
  (let (args)
    (cl-letf (((symbol-function 'ghostel-tmux--spawn-controller)
               (lambda (session tmux-args) (setq args (cons session tmux-args))))
              ((symbol-function 'ghostel-tmux--wait-for-pane) #'ignore))
      (ghostel-tmux-attach "proj.0")
      (should (equal '("proj.0" "attach-session" "-t" "=proj.0:") args)))))

(ert-deftest ghostel-tmux-test-new-escapes-the-session-format ()
  (let (args)
    (cl-letf (((symbol-function 'ghostel-tmux--session-exists-p) #'ignore)
              ((symbol-function 'ghostel-tmux--spawn-controller)
               (lambda (session tmux-args) (setq args (cons session tmux-args))))
              ((symbol-function 'ghostel-tmux--wait-for-pane) #'ignore))
      (ghostel-tmux-new "a#Sb")
      (should (equal '("a#Sb" "new-session" "-As" "a##Sb") args)))))

(ert-deftest ghostel-tmux-test-new-attaches-an-existing-session-exactly ()
  (let (args)
    (cl-letf (((symbol-function 'ghostel-tmux--session-exists-p) #'always)
              ((symbol-function 'ghostel-tmux--spawn-controller)
               (lambda (session tmux-args) (setq args (cons session tmux-args))))
              ((symbol-function 'ghostel-tmux--wait-for-pane) #'ignore))
      (ghostel-tmux-new "proj.0")
      (should (equal '("proj.0" "attach-session" "-t" "=proj.0:") args)))))

(ert-deftest ghostel-tmux-test-new-loads-the-module-once ()
  (let ((loaded 0))
    (cl-letf (((symbol-function 'ghostel-ensure-module)
               (lambda () (cl-incf loaded)))
              ((symbol-function 'ghostel-tmux--session-exists-p) #'always)
              ((symbol-function 'ghostel-tmux--spawn-controller) #'ignore)
              ((symbol-function 'ghostel-tmux--wait-for-pane) #'ignore))
      (ghostel-tmux-new "x")
      (should (= loaded 1)))))

(ert-deftest ghostel-tmux-test-new-checks-the-directory-first ()
  (let ((default-directory "/nonexistent/directory/")
        (asked nil))
    (cl-letf (((symbol-function 'ghostel-tmux--session-exists-p)
               (lambda (_session) (setq asked t))))
      (should-error (ghostel-tmux-new "x") :type 'user-error)
      (should-not asked))))

(ert-deftest ghostel-tmux-test-new-rejects-an-empty-name ()
  ;; `has-session -t =:' succeeds with any session running, and tmux
  ;; reports a new session without a name in a form we cannot parse.
  (cl-letf (((symbol-function 'ghostel-tmux--check-spawn)
             (lambda () (error "Went on"))))
    (should-error (ghostel-tmux-new "") :type 'user-error)))

(ert-deftest ghostel-tmux-test-session-exists-p-asks-for-the-exact-name ()
  (let (args)
    (cl-letf (((symbol-function 'ghostel-tmux--process-file)
               (lambda (&rest rest) (setq args rest) '(1 . ""))))
      (should-not (ghostel-tmux--session-exists-p "proj.0"))
      (should (equal '("has-session" "-t" "=proj.0:") args)))))

(ert-deftest ghostel-tmux-test-session-exists-p-survives-a-missing-tmux ()
  (cl-letf (((symbol-function 'ghostel-tmux--process-file)
             (lambda (&rest _) (signal 'file-missing '("no tmux")))))
    (should-not (ghostel-tmux--session-exists-p "x"))))

(ert-deftest ghostel-tmux-test-literal-format ()
  (dolist (case '(("plain" . "plain")
                  ("a#Sb" . "a##Sb")
                  ("a##b" . "a####b")
                  ("#" . "##")
                  ("e#[b" . "e#[b")
                  ("e##[b" . "e##[b")
                  ("e#[x]#S" . "e#[x]##S")))
    (should (equal (cdr case) (ghostel-tmux--literal-format (car case))))))

(ert-deftest ghostel-tmux-test-takeover-mode-sets-control-function ()
  (let ((ghostel-control-mode-function nil)
        (ghostel-tmux-takeover-mode nil))
    (ghostel-tmux-takeover-mode 1)
    (should (eq #'ghostel-tmux--control ghostel-control-mode-function))
    (ghostel-tmux-takeover-mode -1)
    (should (eq #'ghostel-tmux--control ghostel-control-mode-function))))

(ert-deftest ghostel-tmux-test-unload-removes-the-control-function ()
  (let ((ghostel-control-mode-function #'ghostel-tmux--control))
    (should-not (ghostel-tmux-unload-function))
    (should-not ghostel-control-mode-function))
  (let ((ghostel-control-mode-function #'ignore))
    (ghostel-tmux-unload-function)
    (should (eq #'ignore ghostel-control-mode-function))))

(ert-deftest ghostel-tmux-test-takeover-mode-off-declines-new-frames ()
  (ghostel-tmux-test--with-host host
    (let ((ghostel-tmux-takeover-mode nil)
          (original (buffer-name host)))
      (ghostel-tmux--control 'enter nil)
      (should-not ghostel-tmux--dcs-active)
      (should (equal original (buffer-name host))))))

(ert-deftest ghostel-tmux-test-takeover-mode-off-keeps-running-session ()
  (ghostel-tmux-test--with-host host
    (let ((ghostel-control-mode-function nil)
          (ghostel-tmux-takeover-mode nil)
          (original (buffer-name host)))
      (ghostel-tmux-takeover-mode 1)
      (funcall ghostel-control-mode-function 'enter nil)
      (should ghostel-tmux--dcs-active)
      (ghostel-tmux-takeover-mode -1)
      (funcall ghostel-control-mode-function 'data "%exit\n")
      (should ghostel-tmux--closing)
      (funcall ghostel-control-mode-function 'exit nil)
      (should-not ghostel-tmux--dcs-active)
      (should (equal original (buffer-name host))))))

(ert-deftest ghostel-tmux-test-dcs-data-routes-to-parser ()
  (ghostel-tmux-test--with-host host
    (setq-local ghostel-tmux--dcs-active t)
    (let ((fed nil))
      (cl-letf (((symbol-function 'ghostel-tmux--feed-bytes)
                 (lambda (s) (push s fed))))
        (ghostel-tmux--dcs-data "%session-changed $0 demo\r\n"))
      (should (equal fed (list "%session-changed $0 demo\r\n"))))))

(ert-deftest ghostel-tmux-test-dcs-data-no-op-when-inactive ()
  ;; If we never entered (e.g. customization off), `data' must not
  ;; feed bytes to a nonexistent parser.
  (ghostel-tmux-test--with-host host
    (let ((fed nil))
      (cl-letf (((symbol-function 'ghostel-tmux--feed-bytes)
                 (lambda (s) (push s fed))))
        (ghostel-tmux--dcs-data "%anything\r\n"))
      (should (null fed)))))

(ert-deftest ghostel-tmux-test-dcs-exit-tears-down ()
  (ghostel-tmux-test--with-host host
    (setq-local ghostel-tmux--dcs-active t)
    (let ((exited-buf nil))
      (cl-letf (((symbol-function 'ghostel-tmux--exit-control-mode)
                 (lambda (buf) (setq exited-buf buf))))
        (ghostel-tmux--dcs-exit))
      (should (eq exited-buf host))
      (should-not ghostel-tmux--dcs-active))))

(ert-deftest ghostel-tmux-test-dcs-exit-no-op-when-inactive ()
  (ghostel-tmux-test--with-host host
    (let ((exited nil))
      (cl-letf (((symbol-function 'ghostel-tmux--exit-control-mode)
                 (lambda (_) (setq exited t))))
        (ghostel-tmux--dcs-exit))
      (should-not exited))))

;; ---------------------------------------------------------------------------
;; Prefix-key dispatch (`C-b ...')
;; ---------------------------------------------------------------------------

(ert-deftest ghostel-tmux-test-prefix-target-formats-pane-id ()
  (with-temp-buffer
    (setq-local ghostel-tmux--pane-id 7)
    (should (equal (ghostel-tmux--prefix-target) "%7")))
  (with-temp-buffer
    (setq-local ghostel-tmux--pane-id nil)
    (should (null (ghostel-tmux--prefix-target)))))

(ert-deftest ghostel-tmux-test-cmd-errors-without-controller ()
  (with-temp-buffer
    (setq-local ghostel-tmux--controller-buffer nil)
    (should-error (ghostel-tmux--cmd "list-sessions")
                  :type 'user-error)))

(ert-deftest ghostel-tmux-test-cmd-sends-to-controller ()
  (let ((sent nil))
    (ghostel-tmux-test--with-controller ctl
      (setq ghostel-tmux--controller-process 'fake-proc)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (push s sent))))
        (with-temp-buffer
          (setq-local ghostel-tmux--controller-buffer ctl)
          (ghostel-tmux--cmd "split-window -h"))))
    (should (equal sent '("split-window -h\n")))))

(ert-deftest ghostel-tmux-test-prefix-split-h-builds-vertical-split ()
  (let ((cmds nil))
    (cl-letf (((symbol-function 'ghostel-tmux--cmd)
               (lambda (c) (push c cmds))))
      (with-temp-buffer
        (setq-local ghostel-tmux--pane-id 3)
        (ghostel-tmux--prefix-split-h)))
    (should (equal cmds '("split-window -v -t %3")))))

(ert-deftest ghostel-tmux-test-prefix-split-v-builds-horizontal-split ()
  (let ((cmds nil))
    (cl-letf (((symbol-function 'ghostel-tmux--cmd)
               (lambda (c) (push c cmds))))
      (with-temp-buffer
        (setq-local ghostel-tmux--pane-id 4)
        (ghostel-tmux--prefix-split-v)))
    (should (equal cmds '("split-window -h -t %4")))))

(ert-deftest ghostel-tmux-test-pane-mode-keeps-tab-line-default ()
  "Pane mode sets the tab function in its own buffer only."
  (with-temp-buffer
    (ghostel-tmux-pane-mode 1)
    (should (eq tab-line-tabs-function #'ghostel-tmux--tab-line-tabs))
    (should-not (eq (default-value 'tab-line-tabs-function)
                    #'ghostel-tmux--tab-line-tabs))
    (ghostel-tmux-pane-mode -1)
    (should-not (local-variable-p 'tab-line-tabs-function))))

(ert-deftest ghostel-tmux-test-session-target-from-controller ()
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--session-id 4)
    (with-temp-buffer
      (setq-local ghostel-tmux--controller-buffer ctl)
      (should (equal (ghostel-tmux--session-target) "$4")))))

(ert-deftest ghostel-tmux-test-session-target-nil-without-controller ()
  (with-temp-buffer
    (setq-local ghostel-tmux--controller-buffer nil)
    (should (null (ghostel-tmux--session-target)))))

(ert-deftest ghostel-tmux-test-window-target-from-buffer-local ()
  (with-temp-buffer
    (setq-local ghostel-tmux--window-id 7)
    (should (equal (ghostel-tmux--window-target) "@7")))
  (with-temp-buffer
    (setq-local ghostel-tmux--window-id nil)
    (should (null (ghostel-tmux--window-target)))))

(defun ghostel-tmux-test--cycle (ctl order command)
  "Run COMMAND in a pane of CTL with windows ORDER; return what reached tmux."
  (let ((cmds nil))
    (with-current-buffer ctl
      (setq ghostel-tmux--window-order order
            ghostel-tmux--session-id 2))
    (cl-letf (((symbol-function 'ghostel-tmux--cmd)
               (lambda (c) (push c cmds))))
      (with-temp-buffer
        (setq-local ghostel-tmux--controller-buffer ctl)
        (funcall command)))
    cmds))

(ert-deftest ghostel-tmux-test-prefix-next-window-leaves-the-order-to-tmux ()
  "`C-t n' sends `next-window'; the visible switch comes with tmux's report."
  (ghostel-tmux-test--with-controller ctl
    (should (equal (ghostel-tmux-test--cycle
                    ctl '(10 20) #'ghostel-tmux--prefix-next-window)
                   '("next-window -t $2")))))

(ert-deftest ghostel-tmux-test-prefix-prev-window-leaves-the-order-to-tmux ()
  "`C-t p' sends `previous-window'."
  (ghostel-tmux-test--with-controller ctl
    (should (equal (ghostel-tmux-test--cycle
                    ctl '(10 20) #'ghostel-tmux--prefix-prev-window)
                   '("previous-window -t $2")))))

(ert-deftest ghostel-tmux-test-prefix-next-window-noop-on-single-window ()
  "With one window tmux would answer with an error: send nothing."
  (ghostel-tmux-test--with-controller ctl
    (should-not (ghostel-tmux-test--cycle
                 ctl '(10) #'ghostel-tmux--prefix-next-window))))

(ert-deftest ghostel-tmux-test-cycle-window-needs-a-controller ()
  "A buffer without a controller says so."
  (with-temp-buffer
    (should-error (ghostel-tmux--prefix-next-window) :type 'user-error)))

(ert-deftest ghostel-tmux-test-switch-pane-sees-a-rename-during-the-prompt ()
  "A pane renamed while the prompt is open is found by its new name."
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*old*"))
          (selected nil))
      (unwind-protect
          (progn
            (with-current-buffer pane
              (setq ghostel-tmux--window-id 0))
            (puthash 5 pane ghostel-tmux--panes)
            (puthash 0 (list :panes (list 5)) ghostel-tmux--windows)
            (setq ghostel-tmux--window-order '(0))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (_prompt table &rest _)
                         (with-current-buffer pane
                           (rename-buffer "*new*"))
                         (should (test-completion "*new*" table))
                         (should-not (test-completion "*old*" table))
                         "*new*"))
                      ((symbol-function 'ghostel-tmux--select-pane-buffer)
                       (lambda (buf) (setq selected buf))))
              (with-temp-buffer
                (setq-local ghostel-tmux--controller-buffer ctl)
                (ghostel-tmux-switch-pane)))
            (should (eq selected pane)))
        (kill-buffer pane)))))

(ert-deftest ghostel-tmux-test-window-rows-set-the-window-order ()
  "Rows list the session's windows in tmux's order."
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--window-order '(0 1 2 3))
    (cl-letf (((symbol-function 'ghostel-tmux--on-layout-change) #'ignore))
      (ghostel-tmux--receive-window-rows
       (concat "@0\t%0\t1\tw\tl\tv\n@3\t%3\t0\tw\tl\tv\n"
               "@1\t%1\t0\tw\tl\tv\n@2\t%2\t0\tw\tl\tv\n")))
    (should (equal ghostel-tmux--window-order '(0 3 1 2)))))

(ert-deftest ghostel-tmux-test-window-rows-list-a-linked-window-once ()
  "A window linked twice has two rows, but one tab per pane."
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer " *ghostel-tmux-test-pane*")))
      (unwind-protect
          (progn
            (with-current-buffer pane
              (setq-local ghostel-tmux--window-id 0))
            (puthash 0 pane ghostel-tmux--panes)
            (puthash 0 (list :panes (list 0)) ghostel-tmux--windows)
            (cl-letf (((symbol-function 'ghostel-tmux--on-layout-change)
                       #'ignore))
              (ghostel-tmux--receive-window-rows
               "@0\t%0\t1\tw\tl\tv\n@1\t%1\t0\tw\tl\tv\n@0\t%0\t0\tw\tl\tv\n"))
            (should (equal ghostel-tmux--window-order '(0 1)))
            (should (equal (ghostel-tmux--pane-list ctl) (list pane))))
        (kill-buffer pane)))))

(ert-deftest ghostel-tmux-test-window-rows-order-in-a-c-locale ()
  "A client without UTF-8 gets the tabs of a row as `_'."
  (ghostel-tmux-test--with-controller ctl
    (cl-letf (((symbol-function 'ghostel-tmux--on-layout-change) #'ignore))
      (ghostel-tmux--receive-window-rows
       "@4_%0_1_my_name_a,80x24,0,0,0_a,80x24,0,0,0\n@2_%1_0_w_a,80x24,0,0,1_\n"))
    (should (equal ghostel-tmux--window-order '(4 2)))))

(ert-deftest ghostel-tmux-test-window-rows-without-a-row-keep-the-order ()
  "A reply with no window row says nothing about the order."
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--window-order '(0 1))
    (ghostel-tmux--receive-window-rows "not a row\n")
    (should (equal ghostel-tmux--window-order '(0 1)))))

(ert-deftest ghostel-tmux-test-prefix-new-window-uses-session-target ()
  (let ((cmds nil))
    (ghostel-tmux-test--with-controller ctl
      (setq ghostel-tmux--session-id 5)
      (cl-letf (((symbol-function 'ghostel-tmux--cmd)
                 (lambda (c) (push c cmds))))
        (with-temp-buffer
          (setq-local ghostel-tmux--controller-buffer ctl
                      ghostel-tmux--pane-id 99)
          (ghostel-tmux--prefix-new-window))))
    (should (equal cmds '("new-window -t $5")))))

(ert-deftest ghostel-tmux-test-prefix-kill-window-uses-window-target ()
  (let ((cmds nil))
    (cl-letf (((symbol-function 'ghostel-tmux--cmd)
               (lambda (c) (push c cmds))))
      (with-temp-buffer
        (setq-local ghostel-tmux--window-id 3
                    ghostel-tmux--pane-id 99)
        (ghostel-tmux--prefix-kill-window)))
    (should (equal cmds '("kill-window -t @3")))))

(ert-deftest ghostel-tmux-test-prefix-kill-pane-still-uses-pane-target ()
  ;; kill-pane really does want the pane target — regression guard.
  (let ((cmds nil))
    (cl-letf (((symbol-function 'ghostel-tmux--cmd)
               (lambda (c) (push c cmds))))
      (with-temp-buffer
        (setq-local ghostel-tmux--pane-id 8)
        (ghostel-tmux--prefix-kill-pane)))
    (should (equal cmds '("kill-pane -t %8")))))

(ert-deftest ghostel-tmux-test-prefix-keymap-bindings-installed ()
  (should (eq (lookup-key ghostel-tmux-prefix-map "\"")
              #'ghostel-tmux--prefix-split-h))
  (should (eq (lookup-key ghostel-tmux-prefix-map "%")
              #'ghostel-tmux--prefix-split-v))
  (should (eq (lookup-key ghostel-tmux-prefix-map "c")
              #'ghostel-tmux--prefix-new-window))
  (should (eq (lookup-key ghostel-tmux-prefix-map "x")
              #'ghostel-tmux--prefix-kill-pane))
  (should (eq (lookup-key ghostel-tmux-prefix-map "n")
              #'ghostel-tmux--prefix-next-window))
  (should (eq (lookup-key ghostel-tmux-prefix-map "d")
              #'ghostel-tmux-detach)))

(ert-deftest ghostel-tmux-test-prefix-keymap-numeric-bindings ()
  (dotimes (n 10)
    (let ((binding (lookup-key ghostel-tmux-prefix-map
                               (kbd (number-to-string n)))))
      (should (commandp binding)))))

(ert-deftest ghostel-tmux-test-pane-mode-map-default-prefix-is-ctrl-t ()
  ;; Default `ghostel-tmux-prefix-key' is "C-t" and routes to the
  ;; prefix sub-keymap.  The keymap is the pane minor-mode's own
  ;; `:keymap', which composes with the local input-mode map via the
  ;; standard minor-mode dispatch.
  (should (equal ghostel-tmux-prefix-key "C-t"))
  (should (eq (keymap-parent (lookup-key ghostel-tmux-pane-mode-map
                                         (kbd "C-t")))
              ghostel-tmux-prefix-map)))

(ert-deftest ghostel-tmux-test-pane-mode-map-prefix-self-binding ()
  ;; `<prefix> <prefix>' sends the prefix byte raw to the underlying
  ;; program, matching tmux's convention.
  (should (eq (lookup-key ghostel-tmux-pane-mode-map (kbd "C-t C-t"))
              #'ghostel-tmux--prefix-self-insert)))

(ert-deftest ghostel-tmux-test-pane-mode-map-customization-rebinds ()
  ;; Setting `ghostel-tmux-prefix-key' via customize moves the binding
  ;; in the pane keymap and in the prefix sub-map's self-binding.
  (let ((orig ghostel-tmux-prefix-key))
    (unwind-protect
        (progn
          (customize-set-variable 'ghostel-tmux-prefix-key "C-b")
          (should-not (lookup-key ghostel-tmux-pane-mode-map (kbd "C-t")))
          (should (eq (lookup-key ghostel-tmux-pane-mode-map (kbd "C-b C-b"))
                      #'ghostel-tmux--prefix-self-insert))
          (should (eq (lookup-key ghostel-tmux-pane-mode-map (kbd "C-b d"))
                      #'ghostel-tmux-detach))
          ;; A prefix that is also a key in the prefix map keeps its
          ;; command once the prefix moves on.
          (customize-set-variable 'ghostel-tmux-prefix-key "C-n")
          (should (eq (lookup-key ghostel-tmux-pane-mode-map (kbd "C-n C-n"))
                      #'ghostel-tmux--prefix-self-insert))
          (customize-set-variable 'ghostel-tmux-prefix-key "C-t")
          (should (eq (lookup-key ghostel-tmux-pane-mode-map (kbd "C-t C-n"))
                      #'ghostel-tmux-next-pane)))
      (customize-set-variable 'ghostel-tmux-prefix-key orig))))

(ert-deftest ghostel-tmux-test-pane-mode-wins-over-local-map ()
  ;; Activating the minor mode in a buffer must make the prefix win
  ;; over a local-map binding for the same key.
  (with-temp-buffer
    (let ((shadow-map (make-sparse-keymap)))
      (define-key shadow-map (kbd "C-t") #'ignore)
      (use-local-map shadow-map)
      (should (eq (key-binding (kbd "C-t")) #'ignore))
      (ghostel-tmux-pane-mode 1)
      (unwind-protect
          (should (eq (key-binding (kbd "C-t d")) #'ghostel-tmux-detach))
        (ghostel-tmux-pane-mode -1)))))

(ert-deftest ghostel-tmux-test-pane-mode-composes-with-local-map ()
  ;; Bindings on the local map for unrelated keys must remain reachable
  ;; — the whole reason we use a minor-mode keymap instead of an
  ;; emulation-alist override.
  (with-temp-buffer
    (let ((local (make-sparse-keymap)))
      (define-key local (kbd "RET") #'newline)
      (use-local-map local))
    (ghostel-tmux-pane-mode 1)
    (unwind-protect
        (progn
          (should (eq (key-binding (kbd "RET")) #'newline))
          (should (eq (key-binding (kbd "C-t d")) #'ghostel-tmux-detach)))
      (ghostel-tmux-pane-mode -1))))

(ert-deftest ghostel-tmux-test-pane-mode-leaves-ghostel-keys-alone ()
  (with-temp-buffer
    (use-local-map ghostel-semi-char-mode-map)
    (ghostel-tmux-pane-mode 1)
    (unwind-protect
        (progn
          (should (eq (key-binding (kbd "C-c C-n")) #'ghostel-next-hyperlink))
          (should (eq (key-binding (kbd "C-t C-n")) #'ghostel-tmux-next-pane)))
      (ghostel-tmux-pane-mode -1))))

;; ---------------------------------------------------------------------------
;; on-window-renamed regression (preserves session name across buffer switch)
;; ---------------------------------------------------------------------------

(ert-deftest ghostel-tmux-test-window-renamed-preserves-session-name ()
  ;; Regression: rename-buffer used to look up session-name AFTER
  ;; switching to the pane buffer, where it's nil.  Pane buffers ended up
  ;; named "*ghostel-tmux:nil:bash%0*" instead of using the session name.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane-buf (generate-new-buffer
                     "*ghostel-tmux:work:initial%5*")))
      (unwind-protect
          (progn
            (with-current-buffer pane-buf
              (setq-local ghostel-tmux--pane-id 5
                          ghostel-tmux--window-id 0))
            (with-current-buffer ctl
              (setq ghostel-tmux--session-name "work")
              (puthash 0 (list :name "initial" :panes (list 5))
                       ghostel-tmux--windows)
              (puthash 5 pane-buf ghostel-tmux--panes)
              (ghostel-tmux--on-window-renamed 0 "renamed"))
            (should (equal (buffer-name pane-buf)
                           "*ghostel-tmux:work:renamed%5*")))
        (when (buffer-live-p pane-buf) (kill-buffer pane-buf))))))

;; ---------------------------------------------------------------------------
;; Display-pane-in-window: terminal sized to window dimensions
;; ---------------------------------------------------------------------------

(ert-deftest ghostel-tmux-test-display-pane-noop-on-dead-window ()
  ;; Should not error when given a dead window or buffer.
  (let ((dead-buf (generate-new-buffer " *gone*")))
    (kill-buffer dead-buf)
    (should-not (ghostel-tmux--display-pane-in-window
                 dead-buf (selected-window)))))

(ert-deftest ghostel-tmux-test-on-layout-change-sizes-hidden-panes-only ()
  ;; Layout-change applies tmux's geometry to panes that aren't
  ;; currently displayed.  For visible panes, the Emacs window size
  ;; is authoritative (via `--apply-pane-size' / zoom) and applying
  ;; layout geometry would fight zoom and shrink the active pane.
  (ghostel-tmux-test--with-controller ctl
    (let ((sizes nil)
          (existing-pane (generate-new-buffer "*ghostel-tmux:s:w%0*"))
          (created-pane nil))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-set-size)
                     (lambda (rows cols)
                       (push (cons cols rows) sizes)))
                    ;; Pretend pane %0 is not visible in any window
                    ;; (tests run in batch mode without windows).
                    ((symbol-function 'get-buffer-window-list)
                     (lambda (&rest _) nil))
                    ((symbol-function 'ghostel-tmux--get-or-create-pane)
                     (lambda (_ctl pid _wid _name)
                       (cond
                        ((= pid 0) existing-pane)
                        (t
                         (or created-pane
                             (setq created-pane
                                   (let ((b (generate-new-buffer
                                             "*ghostel-tmux:s:w%1*")))
                                     (with-current-buffer b
                                       (setq-local ghostel--term 'fake
                                                   ghostel--term-cols 1
                                                   ghostel--term-rows 1
                                                   ghostel-tmux--initialized t))
                                     b))))))))
            (with-current-buffer existing-pane
              (setq-local ghostel--term 'fake
                          ghostel--term-cols 1
                          ghostel--term-rows 1
                          ghostel-tmux--initialized t))
            (with-current-buffer ctl
              (ghostel-tmux--on-layout-change
               0 "1234,80x24,0,0[80x12,0,0,0,80x12,0,12,1]"))
            ;; Hidden panes get the layout geometry applied.
            (should (member (cons 80 12) sizes))
            (should (= (cl-count (cons 80 12) sizes :test #'equal) 2)))
        (when (buffer-live-p existing-pane) (kill-buffer existing-pane))
        (when (buffer-live-p created-pane) (kill-buffer created-pane))))))

(ert-deftest ghostel-tmux-test-on-layout-change-skips-visible-panes ()
  ;; Counterpart to the previous test: a pane that IS in a window
  ;; must NOT be resized by layout-change.
  (ghostel-tmux-test--with-controller ctl
    (let ((sizes nil)
          (visible-pane (generate-new-buffer "*ghostel-tmux:visible%0*")))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-set-size)
                     (lambda (rows cols) (push (cons cols rows) sizes)))
                    ((symbol-function 'get-buffer-window-list)
                     (lambda (buf &rest _)
                       (when (eq buf visible-pane) '(fake-window))))
                    ((symbol-function 'ghostel-tmux--get-or-create-pane)
                     (lambda (_ctl _pid _wid _name) visible-pane)))
            (with-current-buffer visible-pane
              (setq-local ghostel--term 'fake
                          ghostel--term-cols 80
                          ghostel--term-rows 40
                          ghostel-tmux--initialized t))
            (with-current-buffer ctl
              ;; Single visible pane reported by tmux at half-height.
              (ghostel-tmux--on-layout-change
               0 "1234,80x24,0,0,0"))
            (should-not sizes))
        (when (buffer-live-p visible-pane) (kill-buffer visible-pane))))))

(ert-deftest ghostel-tmux-test-follow-active-pane-no-buffer ()
  ;; If the buffer for the active pane doesn't exist yet (e.g.
  ;; `%window-pane-changed' raced ahead of `%layout-change'),
  ;; `--follow-active-pane' must no-op rather than error.  The
  ;; subsequent `--on-layout-change' picks up the deferred redirect.
  (ghostel-tmux-test--with-controller ctl
    (let ((displayed nil))
      (cl-letf (((symbol-function 'ghostel-tmux--display-pane-in-window)
                 (lambda (buf _window) (push buf displayed))))
        (with-current-buffer ctl
          (ghostel-tmux--follow-active-pane 99)
          (should (null displayed)))))))

(ert-deftest ghostel-tmux-test-pane-changed-records-and-follows ()
  ;; `--on-window-pane-changed' updates `--active-pane-id' AND drives
  ;; the Emacs-side switch by calling `--follow-active-pane'.  This is
  ;; the single trigger for visible pane switches under the
  ;; tmux-source-of-truth model.
  (ghostel-tmux-test--with-controller ctl
    (let ((followed nil))
      (cl-letf (((symbol-function 'ghostel-tmux--follow-active-pane)
                 (lambda (pid &rest _) (push pid followed))))
        (with-current-buffer ctl
          (setq ghostel-tmux--current-window-id 7)
          (ghostel-tmux--on-window-pane-changed 7 42))
        (should (equal followed '(42)))
        (with-current-buffer ctl
          (should (= ghostel-tmux--active-pane-id 42)))))))

(ert-deftest ghostel-tmux-test-pane-changed-in-background-window ()
  ;; A pane change in a window the session is not on is remembered for
  ;; later, and leaves the view and the active pane alone.
  (ghostel-tmux-test--with-controller ctl
    (let ((followed nil))
      (cl-letf (((symbol-function 'ghostel-tmux--follow-active-pane)
                 (lambda (pid &rest _) (push pid followed))))
        (puthash 3 (list :name "bg" :panes (list 8 9) :active-pane 8)
                 ghostel-tmux--windows)
        (setq ghostel-tmux--current-window-id 2
              ghostel-tmux--active-pane-id 5)
        (ghostel-tmux--on-window-pane-changed 3 9)
        (should (null followed))
        (should (eql ghostel-tmux--active-pane-id 5))
        (should (eql (plist-get (gethash 3 ghostel-tmux--windows) :active-pane)
                     9))))))

(ert-deftest ghostel-tmux-test-pane-changed-in-foreign-window ()
  ;; tmux reports pane changes of every session on the server.
  (ghostel-tmux-test--with-controller ctl
    (let ((followed nil))
      (cl-letf (((symbol-function 'ghostel-tmux--follow-active-pane)
                 (lambda (pid &rest _) (push pid followed))))
        (setq ghostel-tmux--current-window-id 2
              ghostel-tmux--active-pane-id 5)
        (ghostel-tmux--on-window-pane-changed 40 41)
        (should (null followed))
        (should (eql ghostel-tmux--active-pane-id 5))
        (should-not (gethash 40 ghostel-tmux--windows))))))

(ert-deftest ghostel-tmux-test-cycle-pane-sends-select-pane ()
  ;; `--cycle-pane' (C-t C-n / C-t C-p) sends `select-pane' to tmux
  ;; and shows the pane at once.
  (let ((cmds nil)
        (displayed nil)
        (pane-a (generate-new-buffer "*test-cp-a*"))
        (pane-b (generate-new-buffer "*test-cp-b*")))
    (unwind-protect
        (ghostel-tmux-test--with-controller ctl
          (with-current-buffer pane-a
            (setq-local ghostel-tmux--pane-id 1))
          (with-current-buffer pane-b
            (setq-local ghostel-tmux--pane-id 2
                        ghostel-tmux--controller-buffer ctl))
          (cl-letf (((symbol-function 'ghostel-tmux--cmd)
                     (lambda (c) (push c cmds)))
                    ((symbol-function 'ghostel-tmux--display-pane-in-window)
                     (lambda (buf _w) (push buf displayed)))
                    ((symbol-function 'ghostel-tmux--pane-list)
                     (lambda (_ctl) (list pane-a pane-b))))
            (with-current-buffer pane-a
              (setq-local ghostel-tmux--controller-buffer ctl)
              (ghostel-tmux-next-pane)))
          (should (equal cmds '("select-pane -t %2")))
          (should (equal displayed (list pane-b))))
      (kill-buffer pane-a)
      (kill-buffer pane-b))))

(ert-deftest ghostel-tmux-test-cycle-pane-repeat-steps-on ()
  ;; Repeats step on from the pane last asked for, also while it is not
  ;; shown yet, as in a zoomed tile.
  (let ((cmds nil)
        (panes (list (generate-new-buffer "*test-cr-a*")
                     (generate-new-buffer "*test-cr-b*")
                     (generate-new-buffer "*test-cr-c*"))))
    (unwind-protect
        (ghostel-tmux-test--with-controller ctl
          (cl-loop for buf in panes for id from 1
                   do (with-current-buffer buf
                        (setq-local ghostel-tmux--pane-id id)
                        (setq-local ghostel-tmux--controller-buffer ctl)))
          (cl-letf (((symbol-function 'ghostel-tmux--cmd)
                     (lambda (c) (push c cmds)))
                    ((symbol-function 'ghostel-tmux--display-pane-in-window)
                     #'ignore)
                    ((symbol-function 'ghostel-tmux--pane-list)
                     (lambda (_ctl) panes)))
            (with-current-buffer (car panes)
              (let ((last-command nil))
                (ghostel-tmux-next-pane))
              (let ((last-command 'ghostel-tmux-next-pane))
                (ghostel-tmux-next-pane))
              (let ((last-command 'ghostel-tmux-next-pane))
                (ghostel-tmux-prev-pane))
              (let ((last-command 'ignore))
                (ghostel-tmux-prev-pane))))
          (should (equal (reverse cmds)
                         '("select-pane -t %2" "select-pane -t %3"
                           "select-pane -t %2" "select-pane -t %3"))))
      (mapc #'kill-buffer panes))))

(ert-deftest ghostel-tmux-test-switch-pane-sends-select-pane ()
  ;; The picker sends `select-pane' and shows the pane at once.
  (let ((cmds nil)
        (displayed nil)
        (pane-a (generate-new-buffer "*test-sp-a*"))
        (pane-b (generate-new-buffer "*test-sp-b*")))
    (unwind-protect
        (ghostel-tmux-test--with-controller ctl
          (with-current-buffer pane-a
            (setq-local ghostel-tmux--pane-id 1))
          (with-current-buffer pane-b
            (setq-local ghostel-tmux--pane-id 2
                        ghostel-tmux--controller-buffer ctl))
          (cl-letf (((symbol-function 'ghostel-tmux--cmd)
                     (lambda (c) (push c cmds)))
                    ((symbol-function 'ghostel-tmux--display-pane-in-window)
                     (lambda (buf _w) (push buf displayed)))
                    ((symbol-function 'ghostel-tmux--pane-list)
                     (lambda (_ctl) (list pane-a pane-b)))
                    ((symbol-function 'completing-read)
                     (lambda (_p _c &rest _) (buffer-name pane-b))))
            (with-current-buffer pane-a
              (setq-local ghostel-tmux--controller-buffer ctl)
              (ghostel-tmux-switch-pane)))
          (should (equal cmds '("select-pane -t %2")))
          (should (equal displayed (list pane-b))))
      (kill-buffer pane-a)
      (kill-buffer pane-b))))

(ert-deftest ghostel-tmux-test-layout-change-redirects-after-deferred-pane-changed ()
  ;; Race: `%window-pane-changed' arrives before `%layout-change'
  ;; materializes the new pane buffer.  `--on-window-pane-changed'
  ;; records the active id but its `--follow-active-pane' no-ops
  ;; because no pane buffer exists yet.  When `--on-layout-change'
  ;; subsequently creates the buffer, it must re-trigger the redirect.
  (ghostel-tmux-test--with-controller ctl
    (let ((followed nil)
          (created-pane nil))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--get-or-create-pane)
                     (lambda (_ctl pid _wid _name)
                       (or created-pane
                           (setq created-pane
                                 (let ((b (generate-new-buffer
                                           "*ghostel-tmux:deferred%99*")))
                                   (with-current-buffer b
                                     (setq-local ghostel--term 'fake
                                                 ghostel--term-cols 1
                                                 ghostel--term-rows 1
                                                 ghostel-tmux--initialized t))
                                   (puthash pid b ghostel-tmux--panes)
                                   b)))))
                    ((symbol-function 'ghostel-set-size) #'ignore)
                    ((symbol-function 'get-buffer-window-list)
                     (lambda (&rest _) nil))
                    ((symbol-function 'ghostel-tmux--follow-active-pane)
                     (lambda (pid &rest _) (push pid followed))))
            (with-current-buffer ctl
              (setq ghostel-tmux--current-window-id 0)
              (ghostel-tmux--on-window-pane-changed 0 99)
              (setq followed nil)
              (ghostel-tmux--on-layout-change
               0 "1234,80x24,0,0,99"))
            (should (equal followed '(99))))
        (when (buffer-live-p created-pane) (kill-buffer created-pane))))))

(ert-deftest ghostel-tmux-test-pane-mode-installs-buffer-change-hook ()
  ;; Tab-line clicks (and other paths that call `switch-to-buffer'
  ;; directly) need the resize via `window-buffer-change-functions'.
  (let ((buf (generate-new-buffer "*ghostel-tmux:bch%0*")))
    (unwind-protect
        (with-current-buffer buf
          (ghostel-tmux-pane-mode 1)
          (should (memq #'ghostel-tmux--on-window-buffer-change
                        window-buffer-change-functions)))
      (kill-buffer buf))))

;; Sizing goes through `ghostel-resize-function': core fits the grid to
;; the windows showing the pane and hands the result to this function.
(ert-deftest ghostel-tmux-test-resize-pane-reports-size-to-tmux ()
  (let* ((ctl (generate-new-buffer "*ghostel-tmux:resize-ctl%0*"))
         (pane (generate-new-buffer "*ghostel-tmux:resize-pane%0*"))
         (sent nil)
         (resized nil))
    (unwind-protect
        (progn
          (with-current-buffer ctl
            (setq-local ghostel-tmux--controller-process 'fake-proc))
          (with-current-buffer pane
            (setq-local ghostel-tmux--controller-buffer ctl)
            (cl-letf (((symbol-function 'ghostel-set-size)
                       (lambda (&rest args) (setq resized args)))
                      ((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'process-send-string)
                       (lambda (_proc s) (push s sent))))
              (ghostel-tmux--resize-pane 40 120))
            (should (equal resized '(40 120)))
            (should (equal sent '("refresh-client -C 120,40\n")))))
      (kill-buffer pane)
      (kill-buffer ctl))))

(ert-deftest ghostel-tmux-test-resize-pane-without-controller-resizes-locally ()
  (with-temp-buffer
    (let ((resized nil))
      (cl-letf (((symbol-function 'ghostel-set-size)
                 (lambda (&rest args) (setq resized args))))
        (ghostel-tmux--resize-pane 40 120))
      (should (equal resized '(40 120))))))

(ert-deftest ghostel-tmux-test-on-window-buffer-change-resizes ()
  ;; The window-buffer-change hook routes through `--apply-pane-size'.
  (let ((buf (generate-new-buffer "*ghostel-tmux:bch-r%0*"))
        (resized nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostel-tmux--apply-pane-size)
                   (lambda (b w &rest _) (push (cons b w) resized))))
          (with-current-buffer buf
            (ghostel-tmux-pane-mode 1)
            (setq-local ghostel-tmux--controller-buffer buf)
            (cl-letf (((symbol-function 'window-live-p) (lambda (_) t))
                      ((symbol-function 'window-buffer)
                       (lambda (_) buf)))
              (ghostel-tmux--on-window-buffer-change 'fake-window))
            (should (equal resized
                           (list (cons buf 'fake-window))))))
      (kill-buffer buf))))

(ert-deftest ghostel-tmux-test-on-window-buffer-change-skips-the-lost-buffer ()
  ;; From Emacs 31 the hook also runs for the buffer the window lost.
  (let ((buf (generate-new-buffer "*ghostel-tmux:bch-l%0*"))
        (emacs-major-version 31)
        (resized nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostel-tmux--apply-pane-size)
                   (lambda (b w &rest _) (push (cons b w) resized))))
          (with-current-buffer buf
            (setq-local ghostel-tmux--controller-buffer buf))
          (with-temp-buffer
            (cl-letf (((symbol-function 'window-live-p) (lambda (_) t))
                      ((symbol-function 'window-buffer)
                       (lambda (_) buf)))
              (ghostel-tmux--on-window-buffer-change 'fake-window)))
          (should-not resized))
      (kill-buffer buf))))

(ert-deftest ghostel-tmux-test-on-window-buffer-change-in-another-buffer ()
  ;; Before Emacs 31 a buffer-local hook runs in any current buffer.
  (let ((buf (generate-new-buffer "*ghostel-tmux:bch-o%0*"))
        (emacs-major-version 30)
        (resized nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostel-tmux--apply-pane-size)
                   (lambda (b w &rest _) (push (cons b w) resized))))
          (with-current-buffer buf
            (setq-local ghostel-tmux--controller-buffer buf))
          (with-temp-buffer
            (cl-letf (((symbol-function 'window-live-p) (lambda (_) t))
                      ((symbol-function 'window-buffer)
                       (lambda (_) buf)))
              (ghostel-tmux--on-window-buffer-change 'fake-window)))
          (should (equal resized (list (cons buf 'fake-window)))))
      (kill-buffer buf))))

(ert-deftest ghostel-tmux-test-on-window-buffer-change-skips-a-sealed-pane ()
  (let ((buf (generate-new-buffer "*ghostel-tmux:bch-s%0*"))
        (resized nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostel-tmux--apply-pane-size)
                   (lambda (b w) (push (cons b w) resized)))
                  ((symbol-function 'window-live-p) (lambda (_) t))
                  ((symbol-function 'window-buffer) (lambda (_) buf)))
          (ghostel-tmux--on-window-buffer-change 'fake-window)
          (should-not resized))
      (kill-buffer buf))))

;; ---------------------------------------------------------------------------
;; ghostel-tmux--spawn-controller command construction
;; ---------------------------------------------------------------------------

(ert-deftest ghostel-tmux-test-spawn-controller-attach-cmd ()
  (let ((seen-cmd nil)
        (seen-type nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq seen-cmd (plist-get plist :command)
                       seen-type (plist-get plist :connection-type))
                 'fake-proc))
              ((symbol-function 'set-process-coding-system) #'ignore))
      (let ((buf (ghostel-tmux--spawn-controller
                  "demo" '("attach-session" "-t" "demo"))))
        (kill-buffer buf)))
    (should (equal seen-cmd
                   '("tmux" "-u" "-C" "attach-session" "-t" "demo")))
    (should (eq seen-type 'pipe))))

(ert-deftest ghostel-tmux-test-spawn-controller-new-cmd ()
  (let ((seen-cmd nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq seen-cmd (plist-get plist :command))
                 'fake-proc))
              ((symbol-function 'set-process-coding-system) #'ignore))
      (let ((buf (ghostel-tmux--spawn-controller
                  "fresh" '("new-session" "-As" "fresh"))))
        (kill-buffer buf)))
    (should (equal seen-cmd
                   '("tmux" "-u" "-C" "new-session" "-As" "fresh")))))

;; ---------------------------------------------------------------------------
;; Detach: dedicated vs auto-takeover
;; ---------------------------------------------------------------------------

(ert-deftest ghostel-tmux-test-detach-dedicated-kills-process ()
  ;; Dedicated controller (`ghostel-tmux-control-mode' buffer): the
  ;; tmux -CC subprocess belongs to ghostel-tmux and must be killed.
  (ghostel-tmux-test--with-controller ctl
    (let ((deleted nil)
          (sent nil))
      (with-current-buffer ctl
        (setq ghostel-tmux--controller-process 'fake-proc))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (push s sent)))
                ((symbol-function 'accept-process-output) #'ignore)
                ((symbol-function 'delete-process)
                 (lambda (p) (setq deleted p))))
        (with-current-buffer ctl
          (ghostel-tmux-detach)))
      (should (member "detach-client\n" sent))
      (should (eq deleted 'fake-proc)))))

(ert-deftest ghostel-tmux-test-detach-auto-takeover-preserves-process ()
  ;; Auto-takeover host: the control stream rides the user's shell PTY,
  ;; so detach must send detach-client through the host's terminal and
  ;; NOT delete any process.
  (let ((host (generate-new-buffer "*tmux-host-test*")))
    (unwind-protect
        (let ((deleted nil) (sent nil))
          (with-current-buffer host
            (ghostel-mode)
            (setq ghostel-tmux--dcs-active t
                  ghostel-tmux--panes (make-hash-table)
                  ghostel--term 'fake-term))
          (cl-letf (((symbol-function 'ghostel-send-string)
                     (lambda (s &optional raw) (should raw) (push s sent)))
                    ((symbol-function 'delete-process)
                     (lambda (p) (setq deleted p))))
            (with-current-buffer host
              (ghostel-tmux-detach)))
          (should (member "detach-client\n" sent))
          (should-not deleted))
      (kill-buffer host))))

;; ---------------------------------------------------------------------------
;; exit-control-mode: window-restoration
;; ---------------------------------------------------------------------------

(ert-deftest ghostel-tmux-test-exit-control-mode-seals-panes ()
  (let ((host (generate-new-buffer "*xcm-host*"))
        (pane (generate-new-buffer "*xcm-pane*")))
    (unwind-protect
        (progn
          (with-current-buffer pane
            (ghostel-tmux-pane-mode 1)
            (setq ghostel-pty-out-function #'ignore
                  ghostel-resize-function #'ignore))
          (with-current-buffer host
            (ghostel-mode)
            (setq ghostel-tmux--panes (make-hash-table)
                  ghostel-tmux--windows (make-hash-table))
            (puthash 0 pane ghostel-tmux--panes)
            (ghostel-tmux--exit-control-mode host))
          (should (with-current-buffer pane
                    (and (null ghostel-pty-out-function)
                         (null ghostel-resize-function)
                         buffer-read-only)))
          (should (eq (buffer-local-value 'ghostel-tmux--parser-state host)
                      'preamble))
          (should (null (buffer-local-value 'ghostel-tmux--panes host))))
      (when (buffer-live-p pane) (kill-buffer pane))
      (when (buffer-live-p host) (kill-buffer host)))))

;; ---------------------------------------------------------------------------
;; Unibyte safety / re-entry / unknown pane / list-windows
;; ---------------------------------------------------------------------------

(ert-deftest ghostel-tmux-test-on-output-drops-unknown-pane ()
  ;; A `%output' for a pane id we don't know about must be silently
  ;; dropped (per the documented startup-ordering fragility) rather
  ;; than throwing.
  (ghostel-tmux-test--with-controller ctl
    (with-current-buffer ctl
      (puthash 0 (current-buffer) ghostel-tmux--panes)
      ;; Different pane id than what we registered:
      (should-not (ghostel-tmux--on-output 999 "junk")))))

(ert-deftest ghostel-tmux-test-on-output-before-init-dropped ()
  ;; %output arriving before the capture-pane backfill completes is
  ;; already contained in the `visible' snapshot; replaying it would
  ;; duplicate the bytes.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*ghostel-tmux:pre-init%0*"))
          (written nil))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-write-output)
                     (lambda (data &optional _no-reply) (push data written))))
            (with-current-buffer pane
              (setq-local ghostel--term 'fake))
            (with-current-buffer ctl
              (puthash 0 pane ghostel-tmux--panes)
              (ghostel-tmux--on-output 0 "too-early")
              (should (null written))
              (with-current-buffer pane
                (setq ghostel-tmux--initialized t))
              (ghostel-tmux--on-output 0 "on-time")
              (should (equal written '("on-time")))))
        (when (buffer-live-p pane) (kill-buffer pane))))))

(ert-deftest ghostel-tmux-test-history-decodes-capture-escapes ()
  ;; The backfill is captured with -C: a literal backslash arrives as
  ;; `\\', an SGR ESC as `\033'; trailing blank rows are dropped and
  ;; the pane counts as initialized afterwards.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*ghostel-tmux:raw%0*"))
          (written nil))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-write-output)
                     (lambda (data &optional _no-reply) (push data written))))
            (with-current-buffer pane
              (setq-local ghostel--term 'fake))
            (with-current-buffer ctl
              (puthash 0 pane ghostel-tmux--panes))
            (ghostel-tmux--receive-pane-backfill
             ctl 0 nil nil "literal \\\\033 \\033[31mred\n$ \n\n\n" nil)
            (should (equal written '("literal \\033 \e[31mred\r\n$ ")))
            (should (buffer-local-value 'ghostel-tmux--initialized pane)))
        (when (buffer-live-p pane) (kill-buffer pane))))))

(defun ghostel-tmux-test--pane-state (&rest overrides)
  "Return the reply of a plain pane's state, with OVERRIDES of FIELD VALUE."
  (mapconcat
   (lambda (field)
     (or (plist-get overrides field)
         (pcase field
           ('cursor_flag "1")
           ('wrap_flag "1")
           ('pane_height "3")
           ('scroll_region_lower "2")
           (_ "0"))))
   ghostel-tmux--pane-state-fields ","))

(ert-deftest ghostel-tmux-test-parse-pane-state ()
  "A reply parses by field; a reply of any other shape does not."
  (let ((state (ghostel-tmux--parse-pane-state
                (concat (ghostel-tmux-test--pane-state 'cursor_y "2") "\n"))))
    (should (equal (alist-get 'cursor_y state) "2")))
  (should-not (ghostel-tmux--parse-pane-state "3.4"))
  (should-not (ghostel-tmux--parse-pane-state nil))
  ;; A format this tmux lacks expands to nothing.
  (let ((state (ghostel-tmux--parse-pane-state
                (ghostel-tmux-test--pane-state 'bracket_paste_flag ""
                                               'wrap_flag ""))))
    (should (equal (ghostel-tmux--backfill-data state nil "x" nil 3)
                   "x\e[m\e[2A\e[1G"))))

(ert-deftest ghostel-tmux-test-backfill-keeps-blank-rows-and-places-cursor ()
  "With a state the screen stays whole: the cursor is relative to it."
  (should (equal (ghostel-tmux--backfill-data
                  (ghostel-tmux--parse-pane-state
                   (ghostel-tmux-test--pane-state
                    'cursor_x "4" 'cursor_y "0"))
                  nil "history\n$ \n\n" "$ \n\n" 3)
                 "history\r\n$ \r\n\r\n\e[m\e[2A\e[5G")))

(ert-deftest ghostel-tmux-test-backfill-low-terminal-loses-rows-below-cursor ()
  "A terminal lower than the pane drops rows below the cursor, as tmux does."
  (let ((state (ghostel-tmux--parse-pane-state
                (ghostel-tmux-test--pane-state 'pane_height "5"
                                               'scroll_region_lower "4"
                                               'cursor_x "2" 'cursor_y "1"))))
    (should (equal (ghostel-tmux--backfill-data state nil "a\n$ \n\n\n" nil 3)
                   "a\r\n$ \r\n\e[m\e[1A\e[3G"))
    ;; Never more than are below the cursor.
    (should (equal (ghostel-tmux--backfill-data state nil "a\n$ \n\n\n" nil 1)
                   "a\r\n$ \e[m\e[3G"))
    ;; A taller terminal keeps them all.
    (should (equal (ghostel-tmux--backfill-data state nil "a\n$ \n\n\n" nil 9)
                   "a\r\n$ \r\n\r\n\r\n\e[m\e[3A\e[3G"))
    ;; The blank row of the cursor and those above it stay.
    (should (equal (ghostel-tmux--backfill-data state nil "\n\n\n\n" nil 1)
                   "\r\n\e[m\e[3G"))))

(ert-deftest ghostel-tmux-test-backfill-low-terminal-keeps-text ()
  "A joined line below the cursor is more than one row: it stays."
  (should (equal (ghostel-tmux--backfill-data
                  (ghostel-tmux--parse-pane-state
                   (ghostel-tmux-test--pane-state 'pane_height "4"
                                                  'scroll_region_lower "3"
                                                  'cursor_y "1"))
                  nil "a\n$ joined line" nil 3)
                 "a\r\n$ joined line\e[m\e[2A\e[1G")))

(ert-deftest ghostel-tmux-test-backfill-restores-modes ()
  "Each mode reported on is turned on; what is on by default turned off."
  (let ((data (ghostel-tmux--backfill-data
               (ghostel-tmux--parse-pane-state
                (ghostel-tmux-test--pane-state
                 'insert_flag "1" 'keypad_cursor_flag "1" 'keypad_flag "1"
                 'bracket_paste_flag "1" 'mouse_standard_flag "1"
                 'mouse_button_flag "1" 'mouse_all_flag "1"
                 'mouse_sgr_flag "1" 'mouse_utf8_flag "1"
                 'cursor_flag "0" 'wrap_flag "0"))
               nil "x" nil 3)))
    (dolist (sequence '("\e[4h" "\e[?1h" "\e=" "\e[?2004h" "\e[?1000h"
                        "\e[?1002h" "\e[?1003h" "\e[?1006h" "\e[?1005h"
                        "\e[?25l" "\e[?7l"))
      (should (string-search sequence data)))))

(ert-deftest ghostel-tmux-test-backfill-restores-scroll-region ()
  "Only a region that is not the whole screen is set, keeping the cursor."
  (let ((state (ghostel-tmux--parse-pane-state
                (ghostel-tmux-test--pane-state 'scroll_region_upper "1"))))
    (should (string-suffix-p
             "\e[2A\e[1G\e7\e[2;3r\e8"
             (ghostel-tmux--backfill-data state nil "x" nil 3)))
    ;; tmux resets the region when it resizes the pane to a terminal.
    (should-not (string-search
                 "r" (ghostel-tmux--backfill-data state nil "x" nil 9))))
  (should-not (string-search
               "r" (ghostel-tmux--backfill-data
                    (ghostel-tmux--parse-pane-state
                     (ghostel-tmux-test--pane-state))
                    nil "x" nil 3))))

(ert-deftest ghostel-tmux-test-backfill-recreates-alternate-screen ()
  "History and the primary screen go below the alternate screen."
  (should (equal (ghostel-tmux--backfill-data
                  (ghostel-tmux--parse-pane-state
                   (ghostel-tmux-test--pane-state
                    'alternate_on "1" 'alternate_saved_x "6"
                    'alternate_saved_y "1" 'cursor_x "3" 'cursor_y "2"))
                  "p1\np2\np3" "h1\nh2\na1\n\n" "a1\n\n" 3)
                 (concat "h1\r\nh2\e[m\r\np1\r\np2\r\np3\e[m\e[1A\e[7G"
                         "\e[?1049h\e[2J\e[Ha1\r\n\r\n\e[m\e[4G"))))

(ert-deftest ghostel-tmux-test-backfill-alternate-screen-with-joined-rows ()
  "A wrapped row of the alternate screen is not taken for a history row."
  (should (string-prefix-p
           "h1\e[m\r\np1\r\np2\r\np3\e[m\e[2A\e[1G\e[?1049h\e[2J\e[Hjoined\r\na2\e[m"
           (ghostel-tmux--backfill-data
            (ghostel-tmux--parse-pane-state
             (ghostel-tmux-test--pane-state 'alternate_on "1"))
            "p1\np2\np3" "h1\njoined\na2" "joined\na2" 3))))

(ert-deftest ghostel-tmux-test-backfill-primary-screen-of-another-height ()
  "The saved cursor belongs to the primary screen's rows, not the pane's.
tmux resizes that screen when the pane leaves the alternate one."
  (let ((state (ghostel-tmux--parse-pane-state
                (ghostel-tmux-test--pane-state
                 'alternate_on "1" 'alternate_saved_y "4"
                 'pane_height "2" 'scroll_region_lower "1"))))
    ;; Five rows, the cursor on the last, in a pane now two rows high.
    (should (string-prefix-p
             "p1\r\np2\r\np3\r\np4\r\np5\e[m\e[1G\e[?1049h"
             (ghostel-tmux--backfill-data
              state "p1\np2\np3\np4\np5" "a1\na2" "a1\na2" 2))))
  ;; Rows below the saved cursor go first in a lower terminal.
  (should (string-prefix-p
           "p1\r\n\e[m\e[1A\e[1G\e[?1049h"
           (ghostel-tmux--backfill-data
            (ghostel-tmux--parse-pane-state
             (ghostel-tmux-test--pane-state 'alternate_on "1"
                                            'alternate_saved_y "0"))
            "p1\n\n\n\n" "a1\n\n" "a1\n\n" 2))))

(ert-deftest ghostel-tmux-test-backfill-resets-pen-after-each-capture ()
  "A capture ends in the colors of its last cell, and sets none at its start."
  (let ((data (ghostel-tmux--backfill-data
               (ghostel-tmux--parse-pane-state
                (ghostel-tmux-test--pane-state 'alternate_on "1"))
               "\\033[7mp" "\\033[7mh\n\\033[7ma" "\\033[7ma" 3)))
    (should (string-search "\e[7mh\e[m" data))
    (should (string-search "\e[7mp\e[m" data))
    (should (string-search "\e[7ma\e[m" data))))

(ert-deftest ghostel-tmux-test-backfill-stays-bytes ()
  "UTF-8 rows and an escape above 127 keep their bytes in every path."
  (dolist (state (list (ghostel-tmux-test--pane-state)
                       (ghostel-tmux-test--pane-state 'alternate_on "1")))
    (let ((data (ghostel-tmux--backfill-data
                 (ghostel-tmux--parse-pane-state state)
                 "\303\244" "\303\244\\310\n\n\303\244"
                 "\303\244\\310\n\n\303\244" 3)))
      (should-not (multibyte-string-p data))
      (should (string-search "\303\244\310" data)))))

(ert-deftest ghostel-tmux-test-bootstrap-pane-takes-four-replies ()
  "One write asks for all; the replies arrive in order, then the backfill."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil) (got nil) (ghostel-tmux-history-lines 7))
      (ghostel-tmux-test--with-wire ctl sent
        (cl-letf (((symbol-function 'ghostel-tmux--receive-pane-backfill)
                   (lambda (&rest args) (setq got args))))
          (ghostel-tmux--bootstrap-pane ctl 5)
          (should (= 1 (length sent)))
          (should (string-match-p
                   (concat "\\`display-message -p -t %5 '[^\n]*'\n"
                           "capture-pane -aq -peC -t %5\n"
                           "capture-pane -peJC -t %5 -S -7\n"
                           "capture-pane -peJC -t %5\n\\'")
                   (car sent)))
          (ghostel-tmux-test--feed ctl "%begin 1 1 1\r\nstate\r\n%end 1 1 1\r\n")
          (ghostel-tmux-test--feed ctl "%begin 1 2 1\r\n%error 1 2 1\r\n")
          (ghostel-tmux-test--feed ctl "%begin 1 3 1\r\nrows\r\n%end 1 3 1\r\n")
          (should-not got)
          (ghostel-tmux--queue-command "behind")
          (ghostel-tmux--pump-deferred ctl)
          (should (= 1 (length sent)))
          (ghostel-tmux-test--feed ctl "%begin 1 4 1\r\nscreen\r\n%end 1 4 1\r\n")
          (should (equal got (list ctl 5 "state" nil "rows" "screen")))
          (ghostel-tmux--pump-deferred ctl)
          (should (equal (car sent) "behind\n")))))))

(ert-deftest ghostel-tmux-test-bootstrap-pane-gone-between-replies ()
  "A pane lost between the replies changes nothing for the commands behind."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux--bootstrap-pane ctl 5)
        (ghostel-tmux--queue-command "behind")
        (dotimes (i 4)
          (ghostel-tmux-test--feed
           ctl (format "%%begin 1 %d 1\r\n%%error 1 %d 1\r\n" i i)))
        (ghostel-tmux--pump-deferred ctl)
        (should (equal (car sent) "behind\n"))))))

(ert-deftest ghostel-tmux-test-bootstrap-queued-once-per-pane ()
  ;; A second %layout-change arriving before the first capture-pane
  ;; replies must not queue a duplicate backfill pair.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*ghostel-tmux:boot%0*"))
          (captures 0))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--queue-command)
                     (lambda (cmd &optional _cont)
                       (when (string-search "capture-pane" cmd)
                         (cl-incf captures))))
                    ((symbol-function 'get-buffer-window-list)
                     (lambda (&rest _) nil))
                    ((symbol-function 'ghostel-tmux--get-or-create-pane)
                     (lambda (&rest _) pane)))
            (with-current-buffer pane
              (setq-local ghostel--term 'fake
                          ghostel--term-cols 80
                          ghostel--term-rows 24))
            (with-current-buffer ctl
              (ghostel-tmux--on-layout-change 0 "1234,80x24,0,0,0")
              (ghostel-tmux--on-layout-change 0 "1234,80x24,0,0,0"))
            (should (= captures 1)))
        (when (buffer-live-p pane) (kill-buffer pane))))))

(ert-deftest ghostel-tmux-test-session-renamed-updates-name ()
  ;; %session-renamed must refresh the session label and rename
  ;; existing pane buffers; tmux >= 2.5 prefixes the name with $ID.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*ghostel-tmux:old:sh%0*")))
      (unwind-protect
          (with-current-buffer ctl
            (setq ghostel-tmux--session-name "old")
            (puthash 0 pane ghostel-tmux--panes)
            (puthash 0 (list :name "sh" :panes (list 0))
                     ghostel-tmux--windows)
            (setq ghostel-tmux--session-id 0)
            (ghostel-tmux--dispatch-notification "%session-renamed $0 neu")
            (should (equal ghostel-tmux--session-name "neu"))
            (should (equal (buffer-name pane) "*ghostel-tmux:neu:sh%0*")))
        (when (buffer-live-p pane) (kill-buffer pane))))))

(ert-deftest ghostel-tmux-test-reset-from-pane-buffer ()
  ;; The escape hatch must work from the user-visible pane buffer, not
  ;; only from the hidden controller buffer.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*ghostel-tmux:reset%0*"))
          (deleted nil))
      (unwind-protect
          (progn
            (with-current-buffer pane
              (ghostel-tmux-pane-mode 1)
              (setq-local ghostel-tmux--controller-buffer ctl
                          ghostel-pty-out-function #'ignore))
            (with-current-buffer ctl
              (setq ghostel-tmux--controller-process 'fake-proc)
              (puthash 0 pane ghostel-tmux--panes))
            (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                      ((symbol-function 'delete-process)
                       (lambda (p) (setq deleted p))))
              (with-current-buffer pane
                (ghostel-tmux-reset)))
            (should (eq deleted 'fake-proc))
            (should (with-current-buffer pane
                      (and (null ghostel-pty-out-function)
                           buffer-read-only))))
        (when (buffer-live-p pane) (kill-buffer pane))))))

(ert-deftest ghostel-tmux-test-read-session-fails-cleanly-without-server ()
  ;; tmux exiting non-zero (no server running) must yield the clean
  ;; user-error, never parse stderr text as a session name.
  (cl-letf (((symbol-function 'process-file)
             (lambda (&rest _) 1)))
    (should-error (ghostel-tmux--read-session) :type 'user-error)))

(ert-deftest ghostel-tmux-test-capture-errors-on-failed-capture ()
  ;; capture-pane failing (bad target) must raise a user-error instead
  ;; of rendering tmux's error text as terminal content.
  (cl-letf (((symbol-function 'process-file)
             (lambda (&rest _) 1)))
    (should-error (ghostel-tmux-capture "nope:0.0") :type 'user-error)))

(ert-deftest ghostel-tmux-test-receive-window-rows-handles-spaces ()
  ;; Window names containing spaces (common with auto-rename) must
  ;; round-trip through the tab-delimited list-windows format.
  (ghostel-tmux-test--with-controller ctl
    (let ((row "@7\t%3\t0\tpython -m foo\tbd5b,80x24,0,0,3\t"))
      (cl-letf (((symbol-function 'ghostel-tmux--on-layout-change)
                 (lambda (wid layout &rest _)
                   (should (= wid 7))
                   (should (equal layout "bd5b,80x24,0,0,3")))))
        (ghostel-tmux--receive-window-rows row))
      (should (equal (plist-get (gethash 7 ghostel-tmux--windows) :name)
                     "python -m foo"))
      ;; The window's active pane is known before tmux ever reports a
      ;; change of it.
      (should (eql (plist-get (gethash 7 ghostel-tmux--windows) :active-pane)
                   3)))))

(defmacro ghostel-tmux-test--with-window-pane (ctl pane &rest body)
  "Run BODY in controller CTL with PANE, the buffer of %5, the pane of window @0."
  (declare (indent 2))
  `(let ((,pane (generate-new-buffer "*ghostel-tmux:s:w%5*")))
     (unwind-protect
         (with-current-buffer ,ctl
           (with-current-buffer ,pane
             (setq ghostel-tmux--window-id 0))
           (puthash 5 ,pane ghostel-tmux--panes)
           (puthash 0 (list :name "w" :panes (list 5))
                    ghostel-tmux--windows)
           (setq ghostel-tmux--window-order (list 0))
           ,@body)
       (when (buffer-live-p ,pane) (kill-buffer ,pane)))))

(ert-deftest ghostel-tmux-test-window-close-removes-pane-buffer ()
  "A window that tmux no longer lists loses its panes."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux-test--with-window-pane ctl pane
          (ghostel-tmux-test--feed ctl "%window-close @0\r\n")
          (ghostel-tmux--pump-deferred ctl)
          (should (equal (car sent) "list-windows -F \"#{window_id}\"\n"))
          (should (buffer-live-p pane))
          (ghostel-tmux-test--reply ctl "@1\r\n")
          (should-not (gethash 0 ghostel-tmux--windows))
          (should-not (gethash 5 ghostel-tmux--panes))
          (should-not (memq 0 ghostel-tmux--window-order))
          (should-not (buffer-live-p pane)))))))

(ert-deftest ghostel-tmux-test-window-close-keeps-window-of-session ()
  "A window another session let go of stays, as tmux still lists it."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux-test--with-window-pane ctl pane
          (ghostel-tmux-test--feed ctl "%window-close @0\r\n")
          (ghostel-tmux--pump-deferred ctl)
          (ghostel-tmux-test--reply ctl "@0\r\n@1\r\n")
          (should (gethash 0 ghostel-tmux--windows))
          (should (eq (gethash 5 ghostel-tmux--panes) pane))
          (should (memq 0 ghostel-tmux--window-order))
          (should (buffer-live-p pane)))))))

(ert-deftest ghostel-tmux-test-window-close-keeps-pane-moved-away ()
  "A pane another window has taken outlives the window it left."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux-test--with-window-pane ctl pane
          (with-current-buffer pane
            (setq ghostel-tmux--window-id 1))
          (ghostel-tmux-test--feed ctl "%window-close @0\r\n")
          (ghostel-tmux--pump-deferred ctl)
          (ghostel-tmux-test--reply ctl "@1\r\n")
          (should-not (gethash 0 ghostel-tmux--windows))
          (should (eq (gethash 5 ghostel-tmux--panes) pane))
          (should (buffer-live-p pane)))))))

(ert-deftest ghostel-tmux-test-layout-change-keeps-pane-moved-away ()
  "A pane another window has taken outlives the layout of the window it left."
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-window-pane ctl pane
      (with-current-buffer pane
        (setq ghostel-tmux--window-id 1))
      (let ((other (generate-new-buffer "*ghostel-tmux:s:w%6*")))
        (unwind-protect
            (progn
              (with-current-buffer other
                (setq ghostel-tmux--initialized t))
              (puthash 6 other ghostel-tmux--panes)
              (puthash 0 (list :name "w" :panes (list 5 6))
                       ghostel-tmux--windows)
              (ghostel-tmux--on-layout-change 0 "aaaa,80x24,0,0,6")
              (should (equal (plist-get (gethash 0 ghostel-tmux--windows) :panes)
                             '(6)))
              (should (eq (gethash 5 ghostel-tmux--panes) pane))
              (should (buffer-live-p pane)))
          (kill-buffer other))))))

(ert-deftest ghostel-tmux-test-window-close-keeps-window-on-failed-query ()
  "A failed query says nothing about the window."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux-test--with-window-pane ctl pane
          (ghostel-tmux-test--feed ctl "%window-close @0\r\n")
          (ghostel-tmux--pump-deferred ctl)
          (ghostel-tmux-test--feed ctl "%begin 1 1 1\r\n%error 1 1 1\r\n")
          (should (buffer-live-p pane)))))))

(ert-deftest ghostel-tmux-test-window-close-ignores-unknown-window ()
  "A window of another session costs no query."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux-test--feed ctl "%window-close @9\r\n")
        (ghostel-tmux--pump-deferred ctl)
        (should-not sent)))))

(ert-deftest ghostel-tmux-test-unlinked-window-close-drops-at-once ()
  "`%unlinked-window-close' means the session no longer holds the window."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux-test--with-window-pane ctl pane
          (ghostel-tmux-test--feed ctl "%unlinked-window-close @0\r\n")
          (ghostel-tmux--pump-deferred ctl)
          (should-not sent)
          (should-not (gethash 0 ghostel-tmux--windows))
          (should-not (buffer-live-p pane)))))))

(ert-deftest ghostel-tmux-test-names-decode-from-wire-bytes ()
  "Session and window names on the wire are UTF-8 bytes."
  (let ((name "gr\303\266\303\237e")
        (text "gr\u00f6\u00dfe")
        seen)
    (cl-letf (((symbol-function 'ghostel-tmux--on-session-changed)
               (lambda (_sid n) (push n seen)))
              ((symbol-function 'ghostel-tmux--on-session-renamed)
               (lambda (n) (push n seen)))
              ((symbol-function 'ghostel-tmux--on-window-renamed)
               (lambda (_wid n) (push n seen))))
      (ghostel-tmux--dispatch-notification (concat "%session-changed $1 " name))
      (let ((ghostel-tmux--session-id 1))
        (ghostel-tmux--dispatch-notification
         (concat "%session-renamed $1 " name)))
      (ghostel-tmux--dispatch-notification (concat "%window-renamed @1 " name)))
    (should (equal seen (list text text text)))
    (should (equal (nth 3 (ghostel-tmux--parse-window-row
                           (concat "@1\t%2\t1\t" name "\tlayout\tvisible")))
                   text))))

(ert-deftest ghostel-tmux-test-window-row-of-a-client-without-utf-8 ()
  "tmux sends such a client `_' for every tab and non-ASCII character."
  (should (equal (ghostel-tmux--parse-window-row
                  "@1_%2_1_f_n_ster_bd5b,80x24,0,0,2_bd5b,80x24,0,0,2")
                 '(1 2 t "f_n_ster" "bd5b,80x24,0,0,2" "bd5b,80x24,0,0,2")))
  (should (equal (ghostel-tmux--parse-window-row
                  "@1\t%2\t0\tfoo_bar\tbd5b,80x24,0,0,2\t")
                 '(1 2 nil "foo_bar" "bd5b,80x24,0,0,2" ""))))

(ert-deftest ghostel-tmux-test-process-file-is-utf-8-in-any-locale ()
  "Session names and targets do not depend on the locale's coding system."
  (let (seen)
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest args)
                 (setq seen (list args coding-system-for-read
                                  coding-system-for-write))
                 0)))
      (ghostel-tmux--process-file "list-sessions"))
    (should (equal seen '(("tmux" nil (t nil) nil "-u" "list-sessions")
                          utf-8-unix utf-8-unix)))))

(ert-deftest ghostel-tmux-test-socket-args ()
  "A socket name selects `-L', a path `-S' on the host of the directory."
  (let ((default-directory "/tmp/"))
    (dolist (case `((nil . nil)
                    ("" . nil)
                    ("work" . ("-L" "work"))
                    ("/run/tmux/s" . ("-S" "/run/tmux/s"))
                    ("~/s" . ("-S" ,(expand-file-name "~/s")))))
      (should (equal (ghostel-tmux--socket-args (car case)) (cdr case)))))
  (let ((default-directory "/ssh:ghostel-tmux-test-host:/home/u/"))
    (should (equal (ghostel-tmux--socket-args "/run/tmux/s")
                   '("-S" "/run/tmux/s")))
    (should (equal (ghostel-tmux--socket-args "work") '("-L" "work"))))
  (should-error (ghostel-tmux--socket-args "/ssh:ghostel-tmux-test-host:/s")
                :type 'user-error))

(ert-deftest ghostel-tmux-test-process-file-passes-the-socket ()
  "Out-of-band calls go to the server of `ghostel-tmux-socket'."
  (let ((ghostel-tmux-socket "work")
        seen)
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest args)
                 (setq seen args)
                 (insert "a\n")
                 0)))
      (should (equal (ghostel-tmux--process-file "list-sessions")
                     '(0 . "a\n"))))
    (should (equal seen '("tmux" nil (t nil) nil
                          "-u" "-L" "work" "list-sessions")))))

(ert-deftest ghostel-tmux-test-no-sessions-names-the-server ()
  (let ((ghostel-tmux-socket "work"))
    (cl-letf (((symbol-function 'process-file) (lambda (&rest _) 1)))
      (should (equal (cadr (should-error
                            (let ((ghostel-tmux--server
                                   (ghostel-tmux--resolve-server)))
                              (ghostel-tmux--read-session))
                            :type 'user-error))
                     "Ghostel-tmux: No tmux sessions found on -L work")))))

(ert-deftest ghostel-tmux-test-socket-argument-overrides-the-option ()
  "The SOCKET argument wins; the empty string is the default server."
  (let ((ghostel-tmux-socket "work")
        servers)
    (cl-letf (((symbol-function 'ghostel-tmux--check-spawn) #'ignore)
              ((symbol-function 'ghostel-tmux--session-exists-p) #'always)
              ((symbol-function 'ghostel-tmux--spawn-controller)
               (lambda (&rest _) (push ghostel-tmux--server servers)))
              ((symbol-function 'ghostel-tmux--wait-for-pane) #'ignore))
      (ghostel-tmux-attach "x")
      (ghostel-tmux-attach "x" "other")
      (ghostel-tmux-new "x" ""))
    (should (equal (nreverse servers)
                   '(("tmux" "-L" "work") ("tmux" "-L" "other") ("tmux"))))))

(ert-deftest ghostel-tmux-test-socket-is-connection-local ()
  "A connection-local profile sets the socket and program of a host."
  (let ((connection-local-profile-alist nil)
        (connection-local-criteria-alist nil)
        (default-directory "/ssh:ghostel-tmux-test-host:/"))
    (connection-local-set-profile-variables
     'ghostel-tmux-test-profile
     '((ghostel-tmux-socket . "/run/t.sock")
       (ghostel-tmux-program . "/opt/bin/tmux")))
    (connection-local-set-profiles
     '(:application tramp :machine "ghostel-tmux-test-host")
     'ghostel-tmux-test-profile)
    (with-temp-buffer
      (should (equal (ghostel-tmux--resolve-server)
                     '("/opt/bin/tmux" "-S" "/run/t.sock")))
      (should (equal (ghostel-tmux--resolve-server "") '("/opt/bin/tmux")))
      (should-not (local-variable-p 'ghostel-tmux-socket)))))

(ert-deftest ghostel-tmux-test-prefix-argument-prompts-for-the-socket ()
  "With a prefix argument, the sessions are listed on the socket read first."
  (let (listed spawned)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "work"))
              ((symbol-function 'ghostel-tmux--read-session)
               (lambda () (push ghostel-tmux--server listed) "s"))
              ((symbol-function 'ghostel-tmux--check-spawn) #'ignore)
              ((symbol-function 'ghostel-tmux--spawn-controller)
               (lambda (&rest _) (push ghostel-tmux--server spawned)))
              ((symbol-function 'ghostel-tmux--wait-for-pane) #'ignore))
      (let ((current-prefix-arg '(4)))
        (call-interactively #'ghostel-tmux-attach))
      (let ((current-prefix-arg nil))
        (call-interactively #'ghostel-tmux-attach)))
    (should (equal listed '(("tmux") ("tmux" "-L" "work"))))
    (should (equal spawned '(("tmux") ("tmux" "-L" "work"))))))

(ert-deftest ghostel-tmux-test-controller-keeps-its-server ()
  "The controller runs on the socket and keeps it for later calls."
  (let ((ghostel-tmux-socket "work")
        seen-cmd)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq seen-cmd (plist-get plist :command))
                 'fake-proc))
              ((symbol-function 'set-process-coding-system) #'ignore))
      (let ((buf (ghostel-tmux--spawn-controller "demo" '("attach-session"))))
        (unwind-protect
            (should (equal (buffer-local-value 'ghostel-tmux--server buf)
                           '("tmux" "-L" "work")))
          (kill-buffer buf))))
    (should (equal seen-cmd '("tmux" "-u" "-L" "work" "-C" "attach-session")))))

(ert-deftest ghostel-tmux-test-capture-in-a-live-pane-asks-its-session ()
  "A pane of a live session is captured over the control channel."
  (ghostel-tmux-test--with-controller ctl
    (setq-local ghostel-tmux--dcs-active t)
    (let ((pane (generate-new-buffer " *ghostel-tmux-test-pane*"))
          sent shown)
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-ensure-module) #'ignore)
                    ((symbol-function 'process-file)
                     (lambda (&rest _) (error "Ran tmux")))
                    ((symbol-function 'ghostel-tmux--controller-send)
                     (lambda (data) (push data sent)))
                    ((symbol-function 'ghostel-tmux--show-capture)
                     (lambda (&rest args) (setq shown args))))
            (with-current-buffer pane
              (setq ghostel-tmux--controller-buffer ctl)
              (ghostel-tmux-capture "%3"))
            (should (equal sent
                           (list (format "capture-pane -peJC -S -%d -t \\%%3\n"
                                         ghostel-tmux-history-lines))))
            (ghostel-tmux-test--feed
             ctl "%begin 1 7 1\r\nrow\\033[1m\r\n%end 1 7 1\r\n")
            (should (equal shown '("%3" "row\\033[1m" ""))))
        (kill-buffer pane)))))

(ert-deftest ghostel-tmux-test-capture-without-a-live-session-runs-tmux ()
  "A pane whose session is closing is captured on the configured server."
  (ghostel-tmux-test--with-controller ctl
    (setq-local ghostel-tmux--dcs-active t)
    (setq-local ghostel-tmux--closing t)
    (let ((pane (generate-new-buffer " *ghostel-tmux-test-pane*"))
          (ghostel-tmux-socket "work")
          seen)
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-ensure-module) #'ignore)
                    ((symbol-function 'process-file)
                     (lambda (&rest args) (setq seen args) 1)))
            (with-current-buffer pane
              (setq ghostel-tmux--controller-buffer ctl)
              (should (equal (cadr (should-error (ghostel-tmux-capture "%3")
                                                 :type 'user-error))
                             "Ghostel-tmux: capture-pane failed for %3 on -L work")))
            (should (equal (seq-take (nthcdr 4 seen) 3) '("-u" "-L" "work"))))
        (kill-buffer pane)))))

(ert-deftest ghostel-tmux-test-layout-change-tolerates-empty-raw-flags ()
  ;; A window with no printable flags makes `#{window_raw_flags}'
  ;; expand to "", leaving a trailing space on the notification line.
  ;; The dispatcher must still match and extract the layout.
  (let (seen-wid seen-layout)
    (cl-letf (((symbol-function 'ghostel-tmux--on-layout-change)
               (lambda (wid layout &rest _)
                 (setq seen-wid wid seen-layout layout))))
      (ghostel-tmux--dispatch-notification
       "%layout-change @4 b33d,80x24,0,0,0 b33d,80x24,0,0,0 ")
      (should (= seen-wid 4))
      (should (equal seen-layout "b33d,80x24,0,0,0")))))

(ert-deftest ghostel-tmux-test-sentinel-seals-panes-on-controller-death ()
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*ghostel-tmux-sentinel-pane*")))
      (unwind-protect
          (progn
            (with-current-buffer pane
              (ghostel-tmux-pane-mode 1)
              (setq-local ghostel-pty-out-function #'ignore))
            (with-current-buffer ctl
              (puthash 0 pane ghostel-tmux--panes))
            (cl-letf (((symbol-function 'process-buffer)
                       (lambda (_p) ctl))
                      ((symbol-function 'process-live-p)
                       (lambda (_p) nil)))
              (ghostel-tmux--sentinel 'fake-proc "killed: 9\n"))
            (should (with-current-buffer pane
                      (and (null ghostel-pty-out-function)
                           buffer-read-only))))
        (when (buffer-live-p pane) (kill-buffer pane))))))

(ert-deftest ghostel-tmux-test-sentinel-seals-panes-when-controller-buffer-dead ()
  ;; If the hidden controller buffer was killed before the process
  ;; died, its pane table is gone; the sentinel must still find and
  ;; seal the panes via their back-pointers.
  (let ((dead-ctl (generate-new-buffer " *ghostel-tmux-dead-ctl*"))
        (pane (generate-new-buffer "*ghostel-tmux-orphan-pane*")))
    (unwind-protect
        (progn
          (with-current-buffer pane
            (ghostel-tmux-pane-mode 1)
            (setq-local ghostel-tmux--controller-buffer dead-ctl
                        ghostel-pty-out-function #'ignore))
          (kill-buffer dead-ctl)
          (cl-letf (((symbol-function 'process-buffer)
                     (lambda (_p) dead-ctl))
                    ((symbol-function 'process-live-p)
                     (lambda (_p) nil)))
            (ghostel-tmux--sentinel 'fake-proc "killed: 9\n"))
          (should (with-current-buffer pane
                    (and (null ghostel-pty-out-function)
                         buffer-read-only))))
      (when (buffer-live-p pane) (kill-buffer pane))
      (when (buffer-live-p dead-ctl) (kill-buffer dead-ctl)))))


(ert-deftest ghostel-tmux-test-sentinel-kills-controller-buffer ()
  "A controller without a process has no further use."
  (let ((ctl (generate-new-buffer " *ghostel-tmux-test-ctl*")))
    (unwind-protect
        (progn
          (with-current-buffer ctl
            (ghostel-tmux-control-mode))
          (cl-letf (((symbol-function 'process-buffer) (lambda (_p) ctl))
                    ((symbol-function 'process-live-p) (lambda (_p) nil)))
            (ghostel-tmux--sentinel 'fake-proc "finished\n"))
          (should-not (buffer-live-p ctl)))
      (when (buffer-live-p ctl) (kill-buffer ctl)))))

(ert-deftest ghostel-tmux-test-startup-error-is-the-exit-reason ()
  "tmux's own failed startup command says why it leaves."
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--feed
     ctl (concat "\eP1000p%begin 1 1 0\r\ncan't find session: x\r\n"
                 "%error 1 1 0\r\n%exit\r\n"))
    (should (equal ghostel-tmux--exit-reason "can't find session: x"))
    (should (equal (ghostel-tmux--exit-suffix) ": can't find session: x"))))

(ert-deftest ghostel-tmux-test-startup-error-is-decoded ()
  "The reason arrives as UTF-8 bytes."
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--feed
     ctl (concat "\eP1000p%begin 1 1 0\r\n"
                 (encode-coding-string "can't find session: été" 'utf-8)
                 "\r\n%error 1 1 0\r\n"))
    (should (equal ghostel-tmux--exit-reason "can't find session: été"))))

(ert-deftest ghostel-tmux-test-exit-message-names-the-reason ()
  "`%exit' after a failed startup says why."
  (ghostel-tmux-test--with-controller ctl
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (ghostel-tmux-test--feed
         ctl (concat "\eP1000p%begin 1 1 0\r\nno sessions\r\n"
                     "%error 1 1 0\r\n%exit\r\n")))
      (should (member "Ghostel-tmux: tmux exited: no sessions" messages)))))

(ert-deftest ghostel-tmux-test-takeover-reports-and-forgets-the-reason ()
  "A failed attach in a host says why; the next attach starts without it."
  (ghostel-tmux-test--with-host host
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (ghostel-tmux--enter-control-mode host)
        (ghostel-tmux-test--feed
         host (concat "%begin 1 1 0\r\nno sessions\r\n"
                      "%error 1 1 0\r\n%exit\r\n"))
        (ghostel-tmux--exit-control-mode host)
        (should (seq-find (lambda (m)
                            (and (string-search "Exited control mode" m)
                                 (string-suffix-p ": no sessions" m)))
                          messages))
        (ghostel-tmux--enter-control-mode host)
        (should-not ghostel-tmux--exit-reason)
        (ghostel-tmux--exit-control-mode host)))))

(ert-deftest ghostel-tmux-test-command-error-is-no-exit-reason ()
  "The failure of a command of ours says nothing about leaving."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux--queue-command "x")
        (ghostel-tmux-test--feed
         ctl "%begin 1 1 1\r\nnope\r\n%error 1 1 1\r\n")
        (should-not ghostel-tmux--exit-reason)
        (should (equal (ghostel-tmux--exit-suffix) ""))))))

(ert-deftest ghostel-tmux-test-startup-error-after-attach-is-no-exit-reason ()
  "A startup command failing behind a good attach does not end the session."
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--feed
     ctl (concat "\eP1000p%begin 1 1 0\r\n%end 1 1 0\r\n"
                 "%session-changed $0 good\r\n"
                 "%begin 1 2 0\r\ncan't find session: x\r\n%error 1 2 0\r\n"))
    (should-not ghostel-tmux--exit-reason)))

(ert-deftest ghostel-tmux-test-exit-reason-prefers-exit-line ()
  "The reason on the `%exit' line wins."
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--exit-reason "old")
    (ghostel-tmux--on-exit "server exited")
    (should (equal ghostel-tmux--exit-reason "server exited"))
    (ghostel-tmux--on-exit "")
    (should (equal ghostel-tmux--exit-reason "server exited"))))

(defun ghostel-tmux-test--script (body)
  "Return the path of an executable shell script running BODY."
  (let ((path (make-temp-file "ghostel-tmux-script-" nil ".sh")))
    (with-temp-file path
      (insert "#!/bin/sh\n" body "\n"))
    (set-file-modes path #o755)
    path))

(ert-deftest ghostel-tmux-test-entry-points-load-the-module ()
  "Without the module an entry point stops before anything is started."
  (let ((prompted 0)
        (started nil))
    (cl-letf (((symbol-function 'ghostel-ensure-module)
               (lambda ()
                 (cl-incf prompted)
                 (user-error "No module")))
              ((symbol-function 'make-process)
               (lambda (&rest _) (error "Started a process")))
              ((symbol-function 'process-file)
               (lambda (&rest _) (setq started t) 1)))
      (let ((buffers (buffer-list)))
        (should-error (ghostel-tmux-attach "x") :type 'user-error)
        (should-error (ghostel-tmux-new "x") :type 'user-error)
        (should-error (ghostel-tmux-capture "%1") :type 'user-error)
        (should (equal (buffer-list) buffers))))
    (should (= prompted 3))
    (should-not started)))

(ert-deftest ghostel-tmux-test-spawn-honours-a-remote-directory ()
  "tmux runs where the session list came from, on a binary stream."
  (let ((real (symbol-function 'make-process))
        args)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest a)
                 (setq args a)
                 ;; TRAMP's process has the coding of its connection.
                 (funcall real :name "x" :command '("sleep" "10")
                          :coding 'utf-8 :noquery t))))
      (let ((buf (ghostel-tmux--spawn-controller "x" nil)))
        (unwind-protect
            (should (equal (process-coding-system
                            (buffer-local-value
                             'ghostel-tmux--controller-process buf))
                           '(binary . binary)))
          (kill-buffer buf))))
    (should (plist-get args :file-handler))))

(ert-deftest ghostel-tmux-test-attach-needs-the-directory ()
  "A directory that is gone stops the spawn, also on a remote host."
  (cl-letf (((symbol-function 'make-process)
             (lambda (&rest _) (error "Started a process"))))
    (let ((buffers (buffer-list))
          (default-directory "/nonexistent/directory/"))
      (should-error (ghostel-tmux-attach "x") :type 'user-error)
      (should (equal (buffer-list) buffers)))))

(ert-deftest ghostel-tmux-test-spawn-quit-leaves-no-controller ()
  "Quitting a TRAMP login leaves no controller buffer."
  (cl-letf (((symbol-function 'make-process)
             (lambda (&rest _) (signal 'quit nil))))
    (let ((buffers (buffer-list)))
      (should (eq 'quit
                  (condition-case nil
                      (ghostel-tmux--spawn-controller "x" nil)
                    (quit 'quit))))
      (should (equal (buffer-list) buffers)))))

(ert-deftest ghostel-tmux-test-failed-attach-ends-the-wait ()
  "A tmux that dies ends the wait at once and leaves no controller."
  (let* ((script (ghostel-tmux-test--script
                  (concat "printf '\\033P1000p%%begin 1 1 0\\r\\n"
                          "no such session\\r\\n%%error 1 1 0\\r\\n"
                          "%%exit\\r\\n\\033\\\\'")))
         (ghostel-tmux-program script)
         (start (float-time))
         (messages nil))
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (ghostel-tmux-attach "x")
          ;; The sentinel may still be on its way when tmux has just died.
          (let ((deadline (+ (float-time) 2)))
            (while (and (< (float-time) deadline)
                        (seq-find (lambda (b)
                                    (string-prefix-p " *ghostel-tmux-control:x"
                                                     (buffer-name b)))
                                  (buffer-list)))
              (accept-process-output nil 0.02)))
          (should (< (- (float-time) start) 2))
          (should-not (seq-find (lambda (b)
                                  (string-prefix-p " *ghostel-tmux-control:x"
                                                   (buffer-name b)))
                                (buffer-list)))
          (should (seq-find (lambda (m) (string-search "no such session" m))
                            messages))
          (should-not (seq-find (lambda (m) (string-search "Still waiting" m))
                                messages)))
      (delete-file script))))

(ert-deftest ghostel-tmux-test-dead-tmux-ends-the-wait-before-its-sentinel ()
  "A dead tmux whose sentinel has not run yet does not keep the wait spinning."
  (let* ((script (ghostel-tmux-test--script "exit 1"))
         (ghostel-tmux-program script)
         (ctl nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostel-tmux--sentinel) #'ignore))
          (setq ctl (ghostel-tmux--spawn-controller "dead" nil))
          (let ((start (float-time)))
            (ghostel-tmux--wait-for-pane ctl 3)
            (should (< (- (float-time) start) 2))))
      (when (buffer-live-p ctl) (kill-buffer ctl))
      (delete-file script))))

(ert-deftest ghostel-tmux-test-text-before-the-frame-is-the-exit-reason ()
  "What a tmux that never reached control mode printed is reported."
  (let* ((script (ghostel-tmux-test--script "echo 'bad version' >&2; exit 3"))
         (ghostel-tmux-program script)
         (messages nil))
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (ghostel-tmux-attach "pre")
          (let ((deadline (+ (float-time) 2)))
            (while (and (< (float-time) deadline)
                        (not (seq-find (lambda (m) (string-search "code 3" m))
                                       messages)))
              (accept-process-output nil 0.02)))
          (should (member "Ghostel-tmux: exited abnormally with code 3: bad version"
                          messages)))
      (delete-file script))))

(ert-deftest ghostel-tmux-test-failed-spawn-leaves-no-controller ()
  "A tmux that cannot be started signals and leaves no buffer behind."
  (let ((ghostel-tmux-program "ghostel-tmux-test-no-such-program")
        (before (buffer-list)))
    (should-error (ghostel-tmux--spawn-controller "gone" nil)
                  :type 'file-missing)
    (should-not (seq-difference (buffer-list) before))))

(ert-deftest ghostel-tmux-test-pane-at-the-deadline-gets-the-window ()
  "A pane arriving with the last read is shown, not waited for."
  (let* ((script (ghostel-tmux-test--script "sleep 5"))
         (ghostel-tmux-program script)
         (ctl (ghostel-tmux--spawn-controller "edge" nil))
         (pane (generate-new-buffer " *ghostel-tmux-test-pane*"))
         (read nil)
         (shown nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'accept-process-output)
                     (lambda (&rest _)
                       (setq read t)
                       (sleep-for 0.02)))
                    ((symbol-function 'ghostel-tmux--pane-list)
                     (lambda (_controller) (and read (list pane))))
                    ((symbol-function 'ghostel-tmux--display-pane-in-window)
                     (lambda (buf _window) (setq shown buf))))
            (ghostel-tmux--wait-for-pane ctl 0.01))
          (should (eq shown pane))
          (should-not (buffer-local-value 'ghostel-tmux--auto-host-window ctl)))
      (kill-buffer pane)
      (kill-buffer ctl)
      (delete-file script))))

(ert-deftest ghostel-tmux-test-slow-attach-keeps-the-window ()
  "A pane later than the wait still gets the window it was asked for."
  (let* ((script (ghostel-tmux-test--script "sleep 5"))
         (ghostel-tmux-program script)
         (ctl (ghostel-tmux--spawn-controller "slow" nil))
         (messages nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format fmt args) messages))))
            (ghostel-tmux--wait-for-pane ctl 0.1))
          (should (eq (buffer-local-value 'ghostel-tmux--auto-host-window ctl)
                      (selected-window)))
          (should (member "Ghostel-tmux: Still waiting for tmux" messages)))
      (kill-buffer ctl)
      (delete-file script))))

(defconst ghostel-tmux-test--reply-end
  (format "%%begin 9 9 1\r\n%s\r\n%%end 9 9 1\r\n"
          ghostel-tmux--typed-reply-end)
  "The block that answers the marker after a typed command.")

(defmacro ghostel-tmux-test--with-cmd (ctl typed reply &rest body)
  "Run a command in CTL, TYPED or not, answer with REPLY and run BODY."
  (declare (indent 3))
  `(let ((sent nil))
     (ghostel-tmux-test--with-wire ,ctl sent
       (ghostel-tmux--cmd "x y" ,typed)
       (ghostel-tmux-test--feed
        ,ctl (concat ,reply (and ,typed ghostel-tmux-test--reply-end)))
       ,@body)))

(defmacro ghostel-tmux-test--capturing-messages (messages &rest body)
  "Run BODY, collecting what `message' prints in MESSAGES, newest first."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'message)
              (lambda (fmt &rest args)
                (push (apply #'format fmt args) ,messages))))
     ,@body))

(defmacro ghostel-tmux-test--capturing-shown (shown &rest body)
  "Run BODY, collecting what is shown to the user in SHOWN, newest first."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'display-message-or-buffer)
              (lambda (text &rest _) (push text ,shown))))
     ,@body))

(ert-deftest ghostel-tmux-test-cmd-reports-tmux-error-text ()
  "A failed command is reported with tmux's reason."
  (ghostel-tmux-test--with-controller ctl
    (let ((messages nil))
      (ghostel-tmux-test--capturing-messages messages
        (ghostel-tmux-test--with-cmd ctl nil
            "%begin 1 1 1\r\nunknown command: \303\266\r\n%error 1 1 1\r\n"
          (should (equal (car messages)
                         (concat "Ghostel-tmux: Command failed: x y: "
                                 "unknown command: \u00f6"))))))))

(ert-deftest ghostel-tmux-test-cmd-shows-output-of-typed-commands ()
  "Only a typed command shows what it prints, as text."
  (ghostel-tmux-test--with-controller ctl
    (let ((shown nil))
      (ghostel-tmux-test--capturing-shown shown
        (ghostel-tmux-test--with-cmd ctl t
            "%begin 1 1 1\r\ngr\303\266\303\237e\r\n%end 1 1 1\r\n"
          (should (equal shown '("gr\u00f6\u00dfe"))))
        (setq shown nil)
        (ghostel-tmux-test--with-cmd ctl nil
            "%begin 1 1 1\r\nsecret\r\n%end 1 1 1\r\n"
          (should-not shown))
        (ghostel-tmux-test--with-cmd ctl t "%begin 1 1 1\r\n%end 1 1 1\r\n"
          (should-not shown))
        (ghostel-tmux-test--with-cmd ctl t
            "%begin 1 1 1\r\n%message hi there\r\n%end 1 1 1\r\n"
          (should (equal shown '("hi there"))))))))

(ert-deftest ghostel-tmux-test-typed-command-sends-marker-in-one-write ()
  "The marker goes out with the command, as no reply may come in between."
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-cmd ctl t ""
      (should (equal sent
                     (list (format "x y\ndisplay-message -p %s\n"
                                   ghostel-tmux--typed-reply-end)))))))

(ert-deftest ghostel-tmux-test-typed-command-keeps-queue-paired ()
  "All blocks of a typed line are its own, however many reads they take."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil) (shown nil) (behind nil))
      (ghostel-tmux-test--capturing-shown shown
        (ghostel-tmux-test--with-wire ctl sent
          (ghostel-tmux--cmd "a ; b" t)
          (ghostel-tmux--queue-command "c" (lambda (out) (setq behind out)))
          (ghostel-tmux-test--feed ctl "%begin 1 1 1\r\nA\r\n%end 1 1 1\r\n")
          (ghostel-tmux--pump-command-queue)
          (should-not (member "c\n" sent))
          (ghostel-tmux-test--feed ctl "%begin 1 2 1\r\nB\r\n%end 1 2 1\r\n")
          (ghostel-tmux--pump-command-queue)
          (should-not (member "c\n" sent))
          (should-not shown)
          (ghostel-tmux-test--feed ctl ghostel-tmux-test--reply-end)
          (ghostel-tmux--pump-command-queue)
          (should (equal (car sent) "c\n"))
          (should (equal shown '("A\nB")))
          (ghostel-tmux-test--feed ctl "%begin 1 3 1\r\nC\r\n%end 1 3 1\r\n")
          (should (equal behind "C")))))))

(ert-deftest ghostel-tmux-test-typed-command-without-a-block-ends ()
  "A line tmux answers with no block, such as a comment, holds nothing up."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux--cmd "# note" t)
        (ghostel-tmux--queue-command "c")
        (ghostel-tmux-test--feed ctl ghostel-tmux-test--reply-end)
        (ghostel-tmux--pump-command-queue)
        (should (equal (car sent) "c\n"))))))

(ert-deftest ghostel-tmux-test-typed-command-shows-every-block ()
  "Output, `%message' lines and errors of all blocks reach the user."
  (ghostel-tmux-test--with-controller ctl
    (let ((messages nil) (shown nil))
      (ghostel-tmux-test--capturing-messages messages
        (ghostel-tmux-test--capturing-shown shown
          (ghostel-tmux-test--with-cmd ctl t
              (concat "%begin 1 1 1\r\n%end 1 1 1\r\n"
                      "%begin 1 2 1\r\n%message one\r\n%end 1 2 1\r\n"
                      "%begin 1 3 1\r\ntwo\r\n%message 3\r\n%end 1 3 1\r\n"
                      "%begin 1 4 1\r\nbad one\r\n%error 1 4 1\r\n"
                      "%begin 1 5 1\r\nbad two\r\n%error 1 5 1\r\n")
            (should (equal shown '("one\ntwo\n%message 3")))
            (should (equal (car messages)
                           (concat "Ghostel-tmux: Command failed: x y: "
                                   "bad one\nbad two")))))))))

(ert-deftest ghostel-tmux-test-typed-output-buffer ()
  "Long output replaces the last one, even read-only, and moves no focus."
  (ghostel-tmux-test--with-controller ctl
    (let* ((name "*ghostel-tmux-output*")
           (block-of (lambda (line)
                       (concat "%begin 1 1 1\r\n"
                               (mapconcat (lambda (_) line)
                                          (number-sequence 1 500) "\r\n")
                               "\r\n%end 1 1 1\r\n")))
           (window (selected-window))
           (shown (window-buffer window)))
      (unwind-protect
          (progn
            (ghostel-tmux-test--with-cmd ctl t (funcall block-of "first"))
            (with-current-buffer name
              (setq buffer-read-only t))
            (ghostel-tmux-test--with-cmd ctl t (funcall block-of "second")
              (should (eq (current-buffer) ctl)))
            (with-current-buffer name
              (should (string-prefix-p "second\n" (buffer-string)))
              (should-not (string-search "first" (buffer-string))))
            (should (eq (selected-window) window))
            (should (eq (window-buffer window) shown)))
        (delete-other-windows)
        (kill-buffer name)))))

(ert-deftest ghostel-tmux-test-prefix-cmd-sends-no-empty-line ()
  "Neither blank input nor a yanked newline may reach tmux as an empty line."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil) (input "  \n"))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) input)))
        (ghostel-tmux-test--with-wire ctl sent
          (ghostel-tmux--prefix-cmd)
          (should-not sent)
          (setq input "x y\n")
          (ghostel-tmux--prefix-cmd)
          (should (string-prefix-p "x y\ndisplay-message" (car sent)))
          (with-current-buffer ctl
            (setq ghostel-tmux--command-in-flight nil))
          (setq input "a\n\n  \nb")
          (ghostel-tmux--prefix-cmd)
          (should (string-prefix-p "a\nb\ndisplay-message" (car sent))))))))

(ert-deftest ghostel-tmux-test-block-of-tmux-is-no-reply ()
  "A block with flags 0, such as a hook's, leaves the command in flight."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil) (got :none))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux--queue-command "x" (lambda (out) (setq got out)))
        (ghostel-tmux-test--feed ctl "%begin 1 1 0\r\nhook\r\n%end 1 1 0\r\n")
        (should (eq got :none))
        (should ghostel-tmux--command-in-flight)
        (ghostel-tmux-test--feed ctl "%begin 1 2 1\r\nreply\r\n%end 1 2 1\r\n")
        (should (equal got "reply"))))))

(ert-deftest ghostel-tmux-test-pane-list-follows-the-layout ()
  "Panes of a window come in layout order, once each, live ones only."
  (ghostel-tmux-test--with-controller ctl
    (let ((a (generate-new-buffer " a")) (b (generate-new-buffer " b"))
          (dead (generate-new-buffer " dead")))
      (kill-buffer dead)
      (unwind-protect
          (progn
            (puthash 1 a ghostel-tmux--panes)
            (puthash 2 b ghostel-tmux--panes)
            (puthash 3 dead ghostel-tmux--panes)
            (puthash 0 (list :panes (list 2 3 1)) ghostel-tmux--windows)
            (puthash 7 (list :panes (list 1)) ghostel-tmux--windows)
            (setq ghostel-tmux--window-order (list 0 7))
            (should (equal (ghostel-tmux--pane-list ctl) (list b a)))
            (should (equal ghostel-tmux--window-order '(0 7)))
            (should (equal (plist-get (gethash 0 ghostel-tmux--windows) :panes)
                           '(2 3 1))))
        (kill-buffer a)
        (kill-buffer b)))))

(ert-deftest ghostel-tmux-test-killed-pane-buffer-comes-back ()
  "Tmux still has the pane of a buffer the user killed: ask for it again."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil) (pane (generate-new-buffer "*pane*")))
      (ghostel-tmux-test--with-wire ctl sent
        (with-current-buffer pane
          (ghostel-tmux-pane-mode 1)
          (setq ghostel-tmux--controller-buffer ctl
                ghostel-tmux--pane-id 5))
        (puthash 5 pane ghostel-tmux--panes)
        (kill-buffer pane)
        (should-not (gethash 5 ghostel-tmux--panes))
        (should (string-prefix-p "list-windows" (car sent)))))))

(ert-deftest ghostel-tmux-test-removed-pane-does-not-come-back ()
  "A pane tmux removed costs no query."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil) (pane (generate-new-buffer "*pane*")))
      (ghostel-tmux-test--with-wire ctl sent
        (with-current-buffer pane
          (ghostel-tmux-pane-mode 1)
          (setq ghostel-tmux--controller-buffer ctl
                ghostel-tmux--pane-id 5))
        (puthash 5 pane ghostel-tmux--panes)
        (ghostel-tmux--remove-pane ctl 5)
        (should-not sent)))))

(ert-deftest ghostel-tmux-test-sentinel-names-a-detach ()
  "A process ended by `ghostel-tmux-detach' is no failure; other ends are told."
  (pcase-dolist (`(,detaching ,event ,expected)
                 '((t "killed: 9\n" "Ghostel-tmux: Detached")
                   (t "finished\n" "Ghostel-tmux: Detached")
                   (nil "killed: 9\n" "Ghostel-tmux: killed: 9")
                   (nil "finished\n" "Ghostel-tmux: finished")))
    (let ((ctl (generate-new-buffer " *ghostel-tmux-test-ctl*"))
          (messages nil))
      (unwind-protect
          (progn
            (with-current-buffer ctl
              (ghostel-tmux-control-mode)
              (setq ghostel-tmux--detaching detaching))
            (cl-letf (((symbol-function 'process-buffer) (lambda (_p) ctl))
                      ((symbol-function 'process-live-p) (lambda (_p) nil))
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) messages))))
              (ghostel-tmux--sentinel 'fake-proc event))
            (should (equal (car messages) expected)))
        (when (buffer-live-p ctl) (kill-buffer ctl))))))

(ert-deftest ghostel-tmux-test-frame-cut-short-makes-tmux-leave ()
  "A frame ending without `%exit' is followed by an empty line; a normal one not."
  (let ((written nil) (live t))
    (cl-letf (((symbol-function 'ghostel-tmux--exit-control-mode) #'ignore)
              ((symbol-function 'process-live-p) (lambda (_p) live))
              ((symbol-function 'ghostel-send-string)
               (lambda (data &optional raw) (should raw) (push data written))))
      (with-temp-buffer
        (setq-local ghostel--term 'fake)
        (setq ghostel-tmux--dcs-active t)
        (ghostel-tmux--dcs-exit)
        (should (equal written '("\n"))))
      (setq written nil)
      (with-temp-buffer
        (setq-local ghostel--term 'fake)
        (setq ghostel-tmux--dcs-active t
              ghostel-tmux--closing t)
        (ghostel-tmux--dcs-exit)
        (should-not written))
      ;; The host shell is gone: there is nobody to write to.
      (setq live nil)
      (with-temp-buffer
        (setq-local ghostel--term 'fake)
        (setq ghostel-tmux--dcs-active t)
        (ghostel-tmux--dcs-exit)
        (should-not written)))))

(ert-deftest ghostel-tmux-test-second-backfill-is-dropped ()
  "A pane killed with its backfill queued gets two; only the first counts."
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer " pane")) (written nil))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-write-output)
                     (lambda (data &optional _no-reply) (push data written))))
            (puthash 5 pane ghostel-tmux--panes)
            (with-current-buffer pane
              (setq-local ghostel--term 'fake)
              (setq-local ghostel--term-rows 24))
            (ghostel-tmux--receive-pane-backfill ctl 5 nil nil "one" nil)
            (ghostel-tmux--receive-pane-backfill ctl 5 nil nil "two" nil)
            (should (= (length written) 1)))
        (kill-buffer pane)))))

(ert-deftest ghostel-tmux-test-followed-pane-keeps-the-buffer-order ()
  "Showing a pane tmux switched to is no visit: `other-buffer' stays."
  (let ((pane (generate-new-buffer " pane")))
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'ghostel-tmux--apply-pane-size) #'ignore))
            (ghostel-tmux--display-pane-in-window pane (selected-window)))
          (should (eq (window-buffer) pane))
          (should-not (eq (car (buffer-list)) pane)))
      (kill-buffer pane))))

(ert-deftest ghostel-tmux-test-cmd-routes-through-queue ()
  ;; `--cmd' must use the command queue so tmux replies come back
  ;; correlated with the command, not eaten by another block.
  (ghostel-tmux-test--with-controller ctl
    (let (queued)
      (cl-letf (((symbol-function 'ghostel-tmux--queue-command)
                 (lambda (cmd _cont) (push cmd queued)))
                ((symbol-function 'process-live-p) (lambda (_) t)))
        (with-temp-buffer
          (setq-local ghostel-tmux--controller-buffer ctl)
          (with-current-buffer ctl
            (setq ghostel-tmux--controller-process 'fake-proc))
          (ghostel-tmux--cmd "split-window -h")))
      (should (equal queued '("split-window -h"))))))

(ert-deftest ghostel-tmux-test-pane-mode-doesnt-set-dcs-active ()
  ;; Pane buffers derive from `ghostel-mode' but the DCS callbacks
  ;; must NOT enter control mode if invoked there.
  (let ((buf (generate-new-buffer "*ghostel-tmux:auto-skip%0*")))
    (unwind-protect
        (with-current-buffer buf
          (ghostel-tmux-pane-mode 1)
          (should-not ghostel-tmux--dcs-active))
      (kill-buffer buf))))

(ert-deftest ghostel-tmux-test-bytes-to-hex-multibyte-utf8 ()
  ;; Hex encoder must accept a multibyte string (e.g. a paste of
  ;; non-Latin-1 text) and emit its UTF-8 bytes.
  (should (equal (ghostel-tmux--bytes-to-hex "ä")
                 "c3 a4")))

(ert-deftest ghostel-tmux-test-pane-output-drops-replies ()
  ;; Regression: OSC/DA/cursor query replies generated while feeding
  ;; %output to a pane's VT engine would round-trip through send-keys,
  ;; arrive at the pane's stdin after the asking program had exited,
  ;; and leak into the next shell prompt as typed input.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*ghostel-tmux:wi%0*"))
          (no-replies nil))
      (unwind-protect
          (progn
            (with-current-buffer pane
              (setq-local ghostel-tmux--initialized t))
            (puthash 0 pane ghostel-tmux--panes)
            (cl-letf (((symbol-function 'ghostel-write-output)
                       (lambda (_data &optional no-reply)
                         (push no-reply no-replies))))
              (ghostel-tmux--on-output 0 "anything")
              (with-current-buffer pane
                (setq ghostel-tmux--initialized nil))
              (ghostel-tmux--receive-pane-backfill ctl 0 nil nil "rows" nil))
            (should (equal no-replies '(t t))))
        (when (buffer-live-p pane) (kill-buffer pane))))))

(ert-deftest ghostel-tmux-test-ensure-pane-zoomed-skips-single-pane ()
  ;; Single-pane windows must NOT be sent a zoom command (zoom is a
  ;; no-op on single-pane windows but emits a tmux status message).
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (with-current-buffer ctl
        (setq ghostel-tmux--controller-process 'fake-proc)
        (puthash 0 (list :name "w" :panes (list 5)) ghostel-tmux--windows))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (push s sent))))
        (with-current-buffer ctl
          (ghostel-tmux--ensure-pane-zoomed 0 5)))
      (should (null sent)))))

(defmacro ghostel-tmux-test--with-wire (ctl sent &rest body)
  "Run BODY in controller CTL of a two-pane window @0, with SENT the wire.
SENT collects the sent lines, newest first."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
             ((symbol-function 'process-send-string)
              (lambda (_p s) (push s ,sent))))
     (with-current-buffer ,ctl
       (setq ghostel-tmux--controller-process 'fake-proc
             ghostel-tmux--parser-state 'idle)
       (puthash 0 (list :name "w" :panes (list 5 6)) ghostel-tmux--windows)
       ,@body)))

(defun ghostel-tmux-test--reply (ctl payload)
  "Answer the command in flight on CTL with PAYLOAD and run the deferred pump."
  (ghostel-tmux-test--feed
   ctl (format "%%begin 1 1 1\r\n%s%%end 1 1 1\r\n" payload))
  (with-current-buffer ctl
    (ghostel-tmux--pump-deferred ctl)))

(defconst ghostel-tmux-test--zoom-query
  "display-message -p -t %5 '#{window_zoomed_flag}'\n"
  "The line that reads the zoom flag of the window of pane %5.")

(defmacro ghostel-tmux-test--with-shown-pane (ctl pane &rest body)
  "Run BODY in controller CTL with %5, in PANE, shown and active in window @0."
  (declare (indent 2))
  `(let ((,pane (generate-new-buffer "*ghostel-tmux:s:w%5*")))
     (unwind-protect
         (save-window-excursion
           (set-window-buffer (selected-window) ,pane)
           (with-current-buffer ,ctl
             (puthash 5 ,pane ghostel-tmux--panes)
             (setq ghostel-tmux--current-window-id 0
                   ghostel-tmux--active-pane-id 5)
             ,@body))
       (kill-buffer ,pane))))

(ert-deftest ghostel-tmux-test-rezoom-shown-pane-when-unzoomed ()
  "An unzoomed window of the pane shown is zoomed again."
  (let ((ghostel-tmux-split-windows nil))
    (ghostel-tmux-test--with-controller ctl
      (let ((sent nil))
        (ghostel-tmux-test--with-wire ctl sent
          (ghostel-tmux-test--with-shown-pane ctl _pane
            (ghostel-tmux--rezoom-shown-pane 0 "*")
            (should (equal sent (list ghostel-tmux-test--zoom-query)))))))))

(ert-deftest ghostel-tmux-test-rezoom-skips-zoomed-window ()
  "A window zoomed already costs no query."
  (let ((ghostel-tmux-split-windows nil))
    (ghostel-tmux-test--with-controller ctl
      (let ((sent nil))
        (ghostel-tmux-test--with-wire ctl sent
          (ghostel-tmux-test--with-shown-pane ctl _pane
            (ghostel-tmux--rezoom-shown-pane 0 "*Z")
            (should-not sent)))))))

(ert-deftest ghostel-tmux-test-rezoom-skips-other-windows ()
  "Only the current window of the session is zoomed."
  (let ((ghostel-tmux-split-windows nil))
    (ghostel-tmux-test--with-controller ctl
      (let ((sent nil))
        (ghostel-tmux-test--with-wire ctl sent
          (ghostel-tmux-test--with-shown-pane ctl _pane
            (setq ghostel-tmux--current-window-id 1)
            (ghostel-tmux--rezoom-shown-pane 0 "*")
            (should-not sent)))))))

(ert-deftest ghostel-tmux-test-rezoom-skips-hidden-pane ()
  "A pane no window shows needs no zoom."
  (let ((ghostel-tmux-split-windows nil))
    (ghostel-tmux-test--with-controller ctl
      (let ((sent nil))
        (ghostel-tmux-test--with-wire ctl sent
          (ghostel-tmux-test--with-shown-pane ctl _pane
            (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
            (ghostel-tmux--rezoom-shown-pane 0 "*")
            (should-not sent)))))))

(ert-deftest ghostel-tmux-test-rezoom-skips-pane-of-another-window ()
  "A pane on its way into the window, which it does not list yet, waits."
  (let ((ghostel-tmux-split-windows nil))
    (ghostel-tmux-test--with-controller ctl
      (let ((sent nil))
        (ghostel-tmux-test--with-wire ctl sent
          (ghostel-tmux-test--with-shown-pane ctl _pane
            (puthash 0 (list :name "w" :panes (list 6 7)) ghostel-tmux--windows)
            (ghostel-tmux--rezoom-shown-pane 0 "*")
            (should-not sent)))))))

(ert-deftest ghostel-tmux-test-layout-change-rezooms-shown-pane ()
  "A layout without the zoom flag zooms the pane shown, one with it does not."
  (let ((ghostel-tmux-split-windows nil))
    (ghostel-tmux-test--with-controller ctl
      (let ((sent nil)
            (layout "aaaa,80x24,0,0[80x12,0,0,5,80x11,0,13,6]")
            (other (generate-new-buffer "*ghostel-tmux:s:w%6*")))
        (ghostel-tmux-test--with-wire ctl sent
          (ghostel-tmux-test--with-shown-pane ctl pane
            (unwind-protect
                (progn
                  (puthash 6 other ghostel-tmux--panes)
                  (dolist (buf (list pane other))
                    (with-current-buffer buf
                      (setq ghostel-tmux--initialized t)))
                  (ghostel-tmux-test--feed
                   ctl (format "%%layout-change @0 %s bbbb,80x24,0,0,5 *Z\r\n" layout))
                  (ghostel-tmux--pump-deferred ctl)
                  (should-not sent)
                  (ghostel-tmux-test--feed
                   ctl (format "%%layout-change @0 %s %s *\r\n" layout layout))
                  (ghostel-tmux--pump-deferred ctl)
                  (should (equal sent (list ghostel-tmux-test--zoom-query))))
              (kill-buffer other))))))))

(ert-deftest ghostel-tmux-test-reported-pane-replaces-pending-rezoom ()
  "A selection tmux reports stands, also for a pane another window shows.
tmux sends the layout that unzooms before the report of the selection."
  (let ((ghostel-tmux-split-windows nil))
    (ghostel-tmux-test--with-controller ctl
      (let ((sent nil) (other (generate-new-buffer "*ghostel-tmux:s:w%6*")))
        (ghostel-tmux-test--with-wire ctl sent
          (ghostel-tmux-test--with-shown-pane ctl _pane
            (unwind-protect
                (progn
                  (set-window-buffer (split-window) other)
                  (puthash 6 other ghostel-tmux--panes)
                  (ghostel-tmux--rezoom-shown-pane 0 "*")
                  (ghostel-tmux-test--feed ctl "%window-pane-changed @0 %6\r\n")
                  (ghostel-tmux-test--reply ctl "0\r\n")
                  (should (equal (car sent)
                                 "display-message -p -t %6 '#{window_zoomed_flag}'\n"))
                  (ghostel-tmux-test--reply ctl "0\r\n")
                  (should (equal (car sent) "resize-pane -Z -t %6\n"))
                  (should-not (member "resize-pane -Z -t %5\n" sent)))
              (kill-buffer other))))))))

(ert-deftest ghostel-tmux-test-layout-change-passes-flags ()
  "The flags field of `%layout-change' reaches the handler."
  (let (seen)
    (cl-letf (((symbol-function 'ghostel-tmux--on-layout-change)
               (lambda (_wid _layout visible flags)
                 (setq seen (list visible flags)))))
      (ghostel-tmux--dispatch-notification
       "%layout-change @4 b33d,80x24,0,0,0 a,b *Z")
      (should (equal seen '("a,b" "*Z")))
      (ghostel-tmux--dispatch-notification
       "%layout-change @4 b33d,80x24,0,0,0 a,b ")
      (should (equal seen '("a,b" ""))))))

(ert-deftest ghostel-tmux-test-ensure-pane-zoomed-zooms-unzoomed-window ()
  ;; The flag is read from the pane's own window, so a pane shown from
  ;; another tmux window is not unzoomed.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux--ensure-pane-zoomed 0 5)
        (should (equal sent (list ghostel-tmux-test--zoom-query)))
        (ghostel-tmux-test--reply ctl "0\r\n")
        (should (equal (car sent) "resize-pane -Z -t %5\n"))
        (ghostel-tmux-test--reply ctl "")
        (should-not (plist-get (gethash 0 ghostel-tmux--windows) :zooming))))))

(ert-deftest ghostel-tmux-test-ensure-pane-zoomed-keeps-zoomed-window ()
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux--ensure-pane-zoomed 0 5)
        (ghostel-tmux-test--reply ctl "1\r\n")
        (should (equal sent (list ghostel-tmux-test--zoom-query)))
        (should-not (plist-get (gethash 0 ghostel-tmux--windows) :zooming))))))

(ert-deftest ghostel-tmux-test-ensure-pane-zoomed-one-reply-per-command ()
  ;; Every step is a plain command, whose single reply block keeps the
  ;; queue paired.  `if-shell' adds a block when its branch runs.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil) (next nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux--ensure-pane-zoomed 0 5)
        (ghostel-tmux--queue-command "capture-pane" (lambda (out) (setq next out)))
        (ghostel-tmux-test--reply ctl "0\r\n")
        (ghostel-tmux-test--reply ctl "CAPTURED\r\n")
        (should (equal next "CAPTURED"))
        (should (equal (car sent) "resize-pane -Z -t %5\n"))
        (should-not (cl-find "if-shell" sent :test #'string-search))))))

(ert-deftest ghostel-tmux-test-ensure-pane-zoomed-toggles-once ()
  ;; Showing a pane reaches this twice.  Two toggles would unzoom it.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux--ensure-pane-zoomed 0 5)
        (ghostel-tmux--ensure-pane-zoomed 0 5)
        (ghostel-tmux-test--reply ctl "0\r\n")
        (ghostel-tmux--ensure-pane-zoomed 0 5)
        (ghostel-tmux-test--reply ctl "")
        (should-not ghostel-tmux--command-queue)
        (should (equal (reverse sent)
                       (list ghostel-tmux-test--zoom-query
                             "resize-pane -Z -t %5\n")))))))

(ert-deftest ghostel-tmux-test-ensure-pane-zoomed-newer-pane-wins ()
  ;; A pane left before its check returned must not be zoomed: that
  ;; would make it the active pane again.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux--ensure-pane-zoomed 0 5)
        (ghostel-tmux--ensure-pane-zoomed 0 6)
        (ghostel-tmux-test--reply ctl "0\r\n")
        (ghostel-tmux-test--reply ctl "0\r\n")
        (ghostel-tmux-test--reply ctl "")
        (should (equal (car sent) "resize-pane -Z -t %6\n"))
        (should-not (member "resize-pane -Z -t %5\n" sent))
        (should-not (plist-get (gethash 0 ghostel-tmux--windows) :zooming))))))

(ert-deftest ghostel-tmux-test-ensure-pane-zoomed-back-and-forth-toggles-once ()
  ;; 5, 6, 5 within one round trip: only the last check may act.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux--ensure-pane-zoomed 0 5)
        (ghostel-tmux--ensure-pane-zoomed 0 6)
        (ghostel-tmux--ensure-pane-zoomed 0 5)
        (dotimes (_ 3)
          (ghostel-tmux-test--reply ctl "0\r\n"))
        (ghostel-tmux-test--reply ctl "")
        (should (= 1 (cl-count "resize-pane -Z -t %5\n" sent :test #'equal)))
        (should-not (plist-get (gethash 0 ghostel-tmux--windows) :zooming))))))

(ert-deftest ghostel-tmux-test-ensure-pane-zoomed-old-resize-keeps-newer-check ()
  ;; The reply of an older resize must not end a newer zoom of the
  ;; same pane.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux--ensure-pane-zoomed 0 5)
        (ghostel-tmux-test--reply ctl "0\r\n")
        (ghostel-tmux--ensure-pane-zoomed 0 6)
        (ghostel-tmux--ensure-pane-zoomed 0 5)
        (ghostel-tmux-test--reply ctl "")
        (ghostel-tmux-test--reply ctl "0\r\n")
        (ghostel-tmux-test--reply ctl "0\r\n")
        (should (= 2 (cl-count "resize-pane -Z -t %5\n" sent :test #'equal)))))))

(ert-deftest ghostel-tmux-test-selection-cancels-zoom-of-another-pane ()
  ;; `resize-pane -Z' of pane 5 after `select-pane -t %6' would make 5
  ;; active again.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil)
          (pane (generate-new-buffer "*zoom-pane*")))
      (unwind-protect
          (progn
            (with-current-buffer pane
              (setq-local ghostel-tmux--pane-id 6
                          ghostel-tmux--window-id 0
                          ghostel-tmux--controller-buffer ctl))
            (ghostel-tmux-test--with-wire ctl sent
              (ghostel-tmux--ensure-pane-zoomed 0 5)
              (with-current-buffer pane
                (ghostel-tmux--select-pane-buffer pane))
              (ghostel-tmux-test--reply ctl "0\r\n")
              (should-not (member "resize-pane -Z -t %5\n" sent))))
        (kill-buffer pane)))))

(ert-deftest ghostel-tmux-test-selection-keeps-zoom-of-the-same-pane ()
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux--ensure-pane-zoomed 0 5)
        (ghostel-tmux--cancel-zooming 0 5)
        (should (plist-get (gethash 0 ghostel-tmux--windows) :zooming))))))

(ert-deftest ghostel-tmux-test-ensure-pane-zoomed-error-reply ()
  ;; A failed check must not leave the pane marked as in progress.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux--ensure-pane-zoomed 0 5)
        (ghostel-tmux-test--feed ctl "%begin 1 1 1\r\n%error 1 1 1\r\n")
        (should (equal sent (list ghostel-tmux-test--zoom-query)))
        (should-not (plist-get (gethash 0 ghostel-tmux--windows) :zooming))))))

(defmacro ghostel-tmux-test--with-sent (ctl sent &rest body)
  "Run BODY in controller CTL, collecting queued commands in SENT."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'ghostel-tmux--queue-command)
              (lambda (cmd &optional _cont) (push cmd ,sent)))
             ((symbol-function 'ghostel-tmux--cmd)
              (lambda (cmd &optional _typed) (push cmd ,sent))))
     (with-current-buffer ,ctl ,@body)
     (setq ,sent (nreverse ,sent))))

(ert-deftest ghostel-tmux-test-select-pane-skips-reported-pane ()
  ;; Answering tmux's own report with a selection lets two crossing
  ;; selections chase each other forever.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-sent ctl sent
        (setq ghostel-tmux--active-pane-id 5)
        (ghostel-tmux--select-pane 0 5))
      (should (null sent)))))

(ert-deftest ghostel-tmux-test-select-pane-selects-pane-and-window ()
  ;; A pane Emacs chose (tab click, `switch-to-buffer') may live in
  ;; another tmux window.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-sent ctl sent
        (setq ghostel-tmux--active-pane-id 5)
        (ghostel-tmux--select-pane 3 7)
        (should (eql ghostel-tmux--active-pane-id 7)))
      (should (equal sent '("select-pane -t %7" "select-window -t @3"))))))

(ert-deftest ghostel-tmux-test-select-pane-records-current-window ()
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-sent ctl sent
        (ghostel-tmux--select-pane 3 7)
        (should (eql ghostel-tmux--current-window-id 3)))
      (should (equal sent '("select-pane -t %7" "select-window -t @3"))))))

(ert-deftest ghostel-tmux-test-window-rows-seed-current-window ()
  ;; The current window is known before the first pane exists, so
  ;; whatever shows a pane first shows the one tmux has.  The active pane
  ;; is set once all panes exist: a pane shown already keeps its window.
  (ghostel-tmux-test--with-controller ctl
    (let ((seen nil))
      (cl-letf (((symbol-function 'ghostel-tmux--on-layout-change)
                 (lambda (wid &rest _)
                   (push (list wid ghostel-tmux--current-window-id
                               ghostel-tmux--active-pane-id)
                         seen))))
        (ghostel-tmux--receive-window-rows
         "@0\t%0\t0\ta\tbd5b,80x24,0,0,0\t\n@1\t%2\t1\tb\tbd5b,80x24,0,0,2\t"
         t))
      (should (equal (reverse seen) '((0 1 nil) (1 1 nil))))
      (should (eql ghostel-tmux--active-pane-id 2)))))

(ert-deftest ghostel-tmux-test-window-rows-seed-once ()
  ;; A later reply may predate a selection already on its way.
  (ghostel-tmux-test--with-controller ctl
    (cl-letf (((symbol-function 'ghostel-tmux--on-layout-change) #'ignore))
      (setq ghostel-tmux--current-window-id 0
            ghostel-tmux--active-pane-id 0)
      (ghostel-tmux--receive-window-rows
       "@0\t%0\t0\ta\tbd5b,80x24,0,0,0\t\n@1\t%2\t1\tb\tbd5b,80x24,0,0,2\t"
       t)
      (should (eql ghostel-tmux--current-window-id 0))
      (should (eql ghostel-tmux--active-pane-id 0)))))

(ert-deftest ghostel-tmux-test-window-rows-of-a-new-window-do-not-seed ()
  ;; The reply to `list-windows -t @N' can be about another session:
  ;; the old one after a switch, or one grouped with ours.
  (ghostel-tmux-test--with-controller ctl
    (cl-letf (((symbol-function 'ghostel-tmux--on-layout-change) #'ignore))
      (ghostel-tmux--receive-window-rows "@3\t%7\t1\ta\tbd5b,80x24,0,0,7\t")
      (should-not ghostel-tmux--current-window-id)
      (should-not ghostel-tmux--active-pane-id))))

(ert-deftest ghostel-tmux-test-left-new-window-is-not-followed ()
  ;; `new-window ; select-window -t :0': tmux has left the new window
  ;; by the time its rows arrive.
  (ghostel-tmux-test--with-controller ctl
    (let ((followed nil)
          (a (generate-new-buffer "*left-a*"))
          (b (generate-new-buffer "*left-b*")))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--get-or-create-pane)
                     (lambda (&rest _)
                       (with-current-buffer b
                         (setq-local ghostel-tmux--initialized t))
                       b))
                    ((symbol-function 'ghostel-tmux--follow-active-pane)
                     (lambda (pid &rest _) (push pid followed))))
            (puthash 5 a ghostel-tmux--panes)
            (puthash 0 (list :name "w" :panes (list 5) :active-pane 5)
                     ghostel-tmux--windows)
            (setq ghostel-tmux--session-id 1)
            (ghostel-tmux--on-session-window-changed 1 3)
            (ghostel-tmux--on-session-window-changed 1 0)
            (setq followed nil)
            (ghostel-tmux--receive-window-rows
             "@3\t%7\t0\tn\tbd5b,80x24,0,0,7\t")
            (should (null followed))
            (should (eql ghostel-tmux--active-pane-id 5)))
        (kill-buffer a)
        (kill-buffer b)))))

(defmacro ghostel-tmux-test--with-pending-window (spec &rest body)
  "Run BODY with `%session-window-changed' pending for window @3.
SPEC is (FOLLOWED): that variable collects followed panes."
  (declare (indent 1))
  `(let ((,(car spec) nil)
         (pane (generate-new-buffer "*pending*")))
     (unwind-protect
         (cl-letf (((symbol-function 'ghostel-tmux--get-or-create-pane)
                    (lambda (&rest _)
                      (with-current-buffer pane
                        (setq-local ghostel-tmux--initialized t))
                      pane))
                   ((symbol-function 'ghostel-tmux--follow-active-pane)
                    (lambda (pid &rest _) (push pid ,(car spec)))))
           (setq ghostel-tmux--session-id 1)
           (ghostel-tmux--on-session-window-changed 1 3)
           ,@body)
       (kill-buffer pane))))

(ert-deftest ghostel-tmux-test-deferred-follow-takes-reported-pane ()
  ;; `new-window ; split-window': the window's first pane is not the
  ;; active one.
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-pending-window (followed)
      (ghostel-tmux--receive-window-rows
       "@3\t%8\t1\tn\tbd5b,80x24,0,0{40x24,0,0,7,39x24,41,0,8}\t")
      (should (eql ghostel-tmux--active-pane-id 8))
      (should (eql (car followed) 8))
      (should-not ghostel-tmux--pending-follow-wid))))

(ert-deftest ghostel-tmux-test-deferred-follow-waits-for-window-rows ()
  ;; A layout alone does not say which pane is active.
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-pending-window (followed)
      (ghostel-tmux--on-layout-change 3 "bd5b,80x24,0,0,7")
      (should (null followed))
      (should (eql ghostel-tmux--pending-follow-wid 3)))))

(ert-deftest ghostel-tmux-test-session-switch-keeps-the-window ()
  ;; `switch-client': the panes shown are wiped; their Emacs window
  ;; goes on to the new session's pane.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*switch-pane*")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (set-window-buffer (selected-window) pane)
            (puthash 5 pane ghostel-tmux--panes)
            (setq ghostel-tmux--session-id 1)
            (ghostel-tmux--on-session-changed 2 "other")
            (should (eq ghostel-tmux--auto-host-window (selected-window))))
        (when (buffer-live-p pane) (kill-buffer pane))))))

(ert-deftest ghostel-tmux-test-session-switch-prefers-selected-window ()
  (ghostel-tmux-test--with-controller ctl
    (let ((a (generate-new-buffer "*switch-a*"))
          (b (generate-new-buffer "*switch-b*")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (let ((other (split-window)))
              (set-window-buffer (selected-window) a)
              (set-window-buffer other b)
              (puthash 5 a ghostel-tmux--panes)
              (puthash 6 b ghostel-tmux--panes)
              (should (eq (ghostel-tmux--pane-window) (selected-window)))
              (select-window other)
              (with-current-buffer ctl
                (should (eq (ghostel-tmux--pane-window) other)))))
        (when (buffer-live-p a) (kill-buffer a))
        (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest ghostel-tmux-test-second-session-switch-keeps-the-window ()
  ;; Two `%session-changed' back to back: no panes are left for the
  ;; second one to find.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*switch-pane*")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (set-window-buffer (selected-window) pane)
            (puthash 5 pane ghostel-tmux--panes)
            (setq ghostel-tmux--session-id 1)
            (ghostel-tmux--on-session-changed 2 "other")
            (ghostel-tmux--on-session-changed 3 "third")
            (should (eq ghostel-tmux--auto-host-window (selected-window))))
        (when (buffer-live-p pane) (kill-buffer pane))))))

(ert-deftest ghostel-tmux-test-host-window-waits-for-window-rows ()
  ;; A window of another size sends `%layout-change' right after
  ;; `%session-changed', before the rows name the active pane.
  (ghostel-tmux-test--with-controller ctl
    (let ((shown nil)
          (a (generate-new-buffer "*wait-a*"))
          (b (generate-new-buffer "*wait-b*")))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--get-or-create-pane)
                     (lambda (_ctl pid _wid _name)
                       (puthash pid (if (eql pid 1) a b) ghostel-tmux--panes)
                       (with-current-buffer (gethash pid ghostel-tmux--panes)
                         (setq-local ghostel-tmux--initialized t))
                       (gethash pid ghostel-tmux--panes)))
                    ((symbol-function 'ghostel-tmux--follow-active-pane) #'ignore)
                    ((symbol-function 'ghostel-tmux--display-pane-in-window)
                     (lambda (buf _window) (push buf shown))))
            (setq ghostel-tmux--auto-host-window (selected-window))
            (let ((layout "bd5b,80x24,0,0{40x24,0,0,1,39x24,41,0,2}"))
              (ghostel-tmux--on-layout-change 2 layout)
              (should (null shown))
              (should ghostel-tmux--auto-host-window)
              (ghostel-tmux--receive-window-rows
               (format "@2\t%%2\t1\tw\t%s\t" layout) t))
            (should (equal shown (list b)))
            (should (eql ghostel-tmux--active-pane-id 2)))
        (kill-buffer a)
        (kill-buffer b)))))

(ert-deftest ghostel-tmux-test-wipe-panes-forgets-pending-follow ()
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--pending-follow-wid 3)
    (ghostel-tmux--wipe-panes)
    (should-not ghostel-tmux--pending-follow-wid)))

(ert-deftest ghostel-tmux-test-select-window-names-the-session ()
  ;; A bare `@N' can move a session grouped with ours instead.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-sent ctl sent
        (setq ghostel-tmux--session-id 2)
        (ghostel-tmux--select-pane 3 7))
      (should (equal sent '("select-pane -t %7" "select-window -t $2:@3"))))))

(ert-deftest ghostel-tmux-test-wipe-panes-forgets-current-window ()
  ;; Switching sessions seeds the current window anew.
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--current-window-id 3)
    (ghostel-tmux--wipe-panes)
    (should-not ghostel-tmux--current-window-id)))

(ert-deftest ghostel-tmux-test-takeover-host-shows-current-window ()
  ;; The rows name the current window last; the host's window must wait
  ;; for it and take its active pane, not the first pane of either.
  (ghostel-tmux-test--with-controller ctl
    (let ((shown nil)
          (a (generate-new-buffer "*auto-a*"))
          (b (generate-new-buffer "*auto-b*"))
          (c (generate-new-buffer "*auto-c*")))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--get-or-create-pane)
                     (lambda (_ctl pid _wid _name)
                       (puthash pid (pcase pid (0 a) (2 b) (_ c))
                                ghostel-tmux--panes)
                       (with-current-buffer (gethash pid ghostel-tmux--panes)
                         (setq-local ghostel-tmux--initialized t))
                       (gethash pid ghostel-tmux--panes)))
                    ((symbol-function 'ghostel-tmux--follow-active-pane) #'ignore)
                    ((symbol-function 'ghostel-tmux--display-pane-in-window)
                     (lambda (buf _window) (push buf shown))))
            (setq ghostel-tmux--auto-host-window (selected-window))
            (ghostel-tmux--receive-window-rows
             (concat "@0\t%0\t0\ta\tbd5b,80x24,0,0,0\t\n"
                     "@1\t%3\t1\tb\tbd5b,80x24,0,0{40x24,0,0,2,39x24,41,0,3}\t")
             t)
            (should (equal shown (list c)))
            (should-not ghostel-tmux--auto-host-window))
        (kill-buffer a)
        (kill-buffer b)
        (kill-buffer c)))))

(ert-deftest ghostel-tmux-test-session-window-changed-follows-recorded-pane ()
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil) (followed nil)
          (a (generate-new-buffer "*swc-a*"))
          (b (generate-new-buffer "*swc-b*")))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--follow-active-pane)
                     (lambda (pid &rest _) (push pid followed))))
            (ghostel-tmux-test--with-sent ctl sent
              (puthash 5 a ghostel-tmux--panes)
              (puthash 6 b ghostel-tmux--panes)
              (puthash 0 (list :name "w" :panes (list 5 6) :active-pane 6)
                       ghostel-tmux--windows)
              (setq ghostel-tmux--session-id 1)
              (ghostel-tmux--on-session-window-changed 1 0)
              (should (eql ghostel-tmux--current-window-id 0)))
            (should (equal followed '(6)))
            (should (null sent)))
        (kill-buffer a)
        (kill-buffer b)))))

(ert-deftest ghostel-tmux-test-session-window-changed-selects-a-guess ()
  ;; Without a recorded active pane the first pane is a guess; tmux is
  ;; told, so both sides agree.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil) (followed nil)
          (a (generate-new-buffer "*swc-a*")))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--follow-active-pane)
                     (lambda (pid &rest _) (push pid followed))))
            (ghostel-tmux-test--with-sent ctl sent
              (puthash 5 a ghostel-tmux--panes)
              (puthash 0 (list :name "w" :panes (list 5 6))
                       ghostel-tmux--windows)
              (setq ghostel-tmux--session-id 1)
              (ghostel-tmux--on-session-window-changed 1 0))
            (should (equal followed '(5)))
            (should (equal sent '("select-pane -t %5"))))
        (kill-buffer a)))))

(ert-deftest ghostel-tmux-test-session-window-changed-waits-for-joined-pane ()
  ;; `join-pane' reports the pane as active in its new window before
  ;; that window's layout holds it.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil) (followed nil)
          (a (generate-new-buffer "*join-a*"))
          (b (generate-new-buffer "*join-b*")))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--follow-active-pane)
                     (lambda (pid &rest _) (push pid followed)))
                    ((symbol-function 'ghostel-tmux--get-or-create-pane)
                     (lambda (_ctl pid _wid _name)
                       (with-current-buffer (gethash pid ghostel-tmux--panes)
                         (setq-local ghostel-tmux--initialized t))
                       (gethash pid ghostel-tmux--panes))))
            (ghostel-tmux-test--with-sent ctl sent
              (puthash 5 a ghostel-tmux--panes)
              (puthash 6 b ghostel-tmux--panes)
              (puthash 0 (list :name "w" :panes (list 5) :active-pane 5)
                       ghostel-tmux--windows)
              (setq ghostel-tmux--session-id 1
                    ghostel-tmux--current-window-id 1
                    ghostel-tmux--active-pane-id 6)
              (ghostel-tmux--on-window-pane-changed 0 6)
              (ghostel-tmux--on-session-window-changed 1 0)
              (should (null followed))
              (should (eql ghostel-tmux--pending-follow-wid 0))
              (ghostel-tmux--on-layout-change
               0 "bd5b,80x24,0,0[80x12,0,0,5,80x11,0,13,6]")
              (should (eql (car followed) 6))
              (should (eql ghostel-tmux--active-pane-id 6))
              (should-not ghostel-tmux--pending-follow-wid))
            (should-not sent))
        (kill-buffer a)
        (kill-buffer b)))))

;; A floating pane (tmux 3.8) is reported active but is in no layout.
(defmacro ghostel-tmux-test--with-floating-active-pane (&rest body)
  "Run BODY with pane 5 tiled in window 0 and floating pane 9 active there."
  (declare (indent 0))
  `(let ((tiled (generate-new-buffer "*float-tiled*")))
     (unwind-protect
         (cl-letf (((symbol-function 'ghostel-tmux--get-or-create-pane)
                    (lambda (_ctl pid _wid _name)
                      (with-current-buffer (gethash pid ghostel-tmux--panes)
                        (setq-local ghostel-tmux--initialized t))
                      (gethash pid ghostel-tmux--panes))))
           (puthash 5 tiled ghostel-tmux--panes)
           (puthash 0 (list :name "w" :panes (list 5) :active-pane 9)
                    ghostel-tmux--windows)
           (setq ghostel-tmux--session-id 1
                 ghostel-tmux--current-window-id 0)
           ,@body)
       (kill-buffer tiled))))

(ert-deftest ghostel-tmux-test-floating-active-pane-fills-the-kept-window ()
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-floating-active-pane
      (let (shown)
        (cl-letf (((symbol-function 'ghostel-tmux--display-pane-in-window)
                   (lambda (buf _window) (setq shown buf))))
          (setq ghostel-tmux--auto-host-window (selected-window))
          (ghostel-tmux--on-layout-change 0 "b25d,80x24,0,0,5")
          (should (eq shown tiled))
          (should (eql ghostel-tmux--active-pane-id 5))
          (should-not ghostel-tmux--auto-host-window))))))

(ert-deftest ghostel-tmux-test-floating-active-pane-ends-a-pending-follow ()
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-floating-active-pane
      (let (followed)
        (cl-letf (((symbol-function 'ghostel-tmux--follow-active-pane)
                   (lambda (pid &rest _) (push pid followed))))
          (setq ghostel-tmux--pending-follow-wid 0)
          (ghostel-tmux--on-layout-change 0 "b25d,80x24,0,0,5")
          (should-not ghostel-tmux--pending-follow-wid)
          (should (memq 5 followed)))))))

(ert-deftest ghostel-tmux-test-session-window-changed-keeps-a-floating-selection ()
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-floating-active-pane
      (let ((sent nil) (followed nil))
        (cl-letf (((symbol-function 'ghostel-tmux--follow-active-pane)
                   (lambda (pid &rest _) (push pid followed))))
          (ghostel-tmux-test--with-sent ctl sent
            (ghostel-tmux--on-session-window-changed 1 0))
          (should (equal followed '(5)))
          (should (null sent)))))))

(ert-deftest ghostel-tmux-test-lost-active-pane-leaves-the-kept-window ()
  ;; A pane that just left the window is not a floating one: the query
  ;; `--keep-lost-pane-window' asked decides what the window shows.
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-floating-active-pane
      (let (shown)
        (cl-letf (((symbol-function 'ghostel-tmux--display-pane-in-window)
                   (lambda (buf _window) (setq shown buf))))
          (puthash 0 (list :name "w" :panes (list 5 9) :active-pane 9)
                   ghostel-tmux--windows)
          (setq ghostel-tmux--auto-host-window (selected-window))
          (ghostel-tmux--on-layout-change 0 "b25d,80x24,0,0,5")
          (should-not shown)
          (should ghostel-tmux--auto-host-window))))))

(ert-deftest ghostel-tmux-test-joined-pane-without-a-buffer-is-followed ()
  ;; `join-pane' empties the source window first, so the pane reported
  ;; active here has no buffer until the layout of this window arrives.
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-floating-active-pane
      (let ((joined (generate-new-buffer "*float-joined*"))
            (followed nil))
        (unwind-protect
            (cl-letf (((symbol-function 'ghostel-tmux--follow-active-pane)
                       (lambda (pid &rest _) (push pid followed))))
              (ghostel-tmux--on-session-window-changed 1 0)
              (should (eql ghostel-tmux--pending-follow-wid 0))
              (puthash 9 joined ghostel-tmux--panes)
              (ghostel-tmux--on-layout-change
               0 "bd5b,80x24,0,0{40x24,0,0,5,39x24,41,0,9}")
              (should-not ghostel-tmux--pending-follow-wid)
              (should (eql 9 (car followed))))
          (kill-buffer joined))))))

(ert-deftest ghostel-tmux-test-moving-active-pane-leaves-the-kept-window ()
  ;; A pane with a buffer elsewhere is moving in, not floating.
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-floating-active-pane
      (let ((moving (generate-new-buffer "*float-moving*"))
            shown)
        (unwind-protect
            (cl-letf (((symbol-function 'ghostel-tmux--display-pane-in-window)
                       (lambda (buf _window) (setq shown buf))))
              (puthash 9 moving ghostel-tmux--panes)
              (setq ghostel-tmux--auto-host-window (selected-window))
              (ghostel-tmux--on-layout-change 0 "b25d,80x24,0,0,5")
              (should-not shown)
              (should ghostel-tmux--auto-host-window))
          (kill-buffer moving))))))

(ert-deftest ghostel-tmux-test-lost-pane-with-a-floating-successor-asks-the-rows ()
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-floating-active-pane
      (let (asked)
        (cl-letf (((symbol-function 'ghostel-tmux--request-window-rows)
                   (lambda (&rest _) (setq asked t))))
          (setq ghostel-tmux--auto-host-window (selected-window))
          (ghostel-tmux--receive-active-pane "%9\n")
          (should asked))))))

(ert-deftest ghostel-tmux-test-attach-pane-stands-in-for-a-floating-one ()
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-floating-active-pane
      (setq ghostel-tmux--active-pane-id 9)
      (should (eq tiled (ghostel-tmux--attach-pane)))
      (should (eql 5 ghostel-tmux--active-pane-id)))))

(ert-deftest ghostel-tmux-test-wait-for-pane-shows-the-stand-in ()
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-floating-active-pane
      (let ((other (generate-new-buffer "*float-other*"))
            shown)
        (unwind-protect
            (cl-letf (((symbol-function 'ghostel-tmux--pane-list)
                       (lambda (_controller) (list other tiled)))
                      ((symbol-function 'ghostel-tmux--display-pane-in-window)
                       (lambda (buf _window) (setq shown buf))))
              (setq ghostel-tmux--active-pane-id 9)
              (ghostel-tmux--wait-for-pane ctl 0)
              (should (eq shown tiled)))
          (kill-buffer other))))))

(ert-deftest ghostel-tmux-test-select-pane-replaces-a-deferred-follow ()
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-sent ctl sent
        (setq ghostel-tmux--pending-follow-wid 0
              ghostel-tmux--active-pane-id 9)
        (ghostel-tmux--select-pane 0 5))
      (should-not ghostel-tmux--pending-follow-wid))))

(ert-deftest ghostel-tmux-test-killed-stand-in-keeps-its-window ()
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-floating-active-pane
      (let (asked)
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) tiled)
          (with-current-buffer tiled
            (setq-local ghostel-tmux--window-id 0))
          (setq ghostel-tmux--active-pane-id 9)
          (cl-letf (((symbol-function 'ghostel-tmux--queue-command)
                     (lambda (cmd &optional _cont) (setq asked cmd))))
            (ghostel-tmux--remove-pane ctl 5))
          (should (eq ghostel-tmux--auto-host-window (selected-window)))
          (should (string-prefix-p "display-message" asked)))))))

(ert-deftest ghostel-tmux-test-pane-of-another-window-keeps-no-window ()
  ;; With a floating pane active, a pane elsewhere is still background.
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-floating-active-pane
      (save-window-excursion
        (delete-other-windows)
        (set-window-buffer (selected-window) tiled)
        (with-current-buffer tiled
          (setq-local ghostel-tmux--window-id 3))
        (setq ghostel-tmux--active-pane-id 9)
        (ghostel-tmux--remove-pane ctl 5)
        (should-not ghostel-tmux--auto-host-window)))))

(ert-deftest ghostel-tmux-test-floating-only-window-moves-the-session ()
  ;; Only the window is selected: the panes of that window stay as they are.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*float-elsewhere*"))
          (sent nil))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--get-or-create-pane)
                     (lambda (_ctl pid _wid _name)
                       (puthash pid pane ghostel-tmux--panes)
                       (with-current-buffer pane
                         (setq-local ghostel-tmux--initialized t))
                       pane)))
            (ghostel-tmux-test--with-sent ctl sent
              (setq ghostel-tmux--auto-host-window (selected-window)
                    ghostel-tmux--session-id 1)
              (ghostel-tmux--receive-window-rows
               (concat "@0\t%0\t0\ta\tb25d,80x24,0,0,0\t\n"
                       "@1\t%1\t1\tb\t0000,\t")
               t))
            (should (equal sent '("select-window -t $1:@0")))
            (should ghostel-tmux--auto-host-window))
        (kill-buffer pane)))))

(ert-deftest ghostel-tmux-test-failed-move-asks-the-rows-again ()
  ;; The window was gone by the time tmux got the command.
  (ghostel-tmux-test--with-controller ctl
    (let (cont asked)
      (cl-letf (((symbol-function 'ghostel-tmux--queue-command)
                 (lambda (_cmd &optional c) (setq cont c)))
                ((symbol-function 'ghostel-tmux--request-window-rows)
                 (lambda (&rest _) (setq asked t))))
        (puthash 0 (list :panes (list 5)) ghostel-tmux--windows)
        (puthash 1 (list :panes nil) ghostel-tmux--windows)
        (setq ghostel-tmux--window-order '(0 1)
              ghostel-tmux--current-window-id 1
              ghostel-tmux--auto-host-window (selected-window))
        (ghostel-tmux--leave-floating-window)
        (funcall cont "")
        (should-not asked)
        (funcall cont nil)
        (should asked)))))

(ert-deftest ghostel-tmux-test-attach-to-a-floating-only-window-moves-the-session ()
  ;; No pane is shown or selected: the window that tmux moves to decides.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*float-attach*"))
          (sent nil)
          shown)
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--pane-list)
                     (lambda (_controller) (list pane)))
                    ((symbol-function 'ghostel-tmux--display-pane-in-window)
                     (lambda (buf _window) (setq shown buf))))
            (ghostel-tmux-test--with-sent ctl sent
              (puthash 5 pane ghostel-tmux--panes)
              (puthash 0 (list :panes (list 5)) ghostel-tmux--windows)
              (puthash 1 (list :panes nil :active-pane 9) ghostel-tmux--windows)
              (setq ghostel-tmux--window-order '(0 1)
                    ghostel-tmux--session-id 1
                    ghostel-tmux--current-window-id 1
                    ghostel-tmux--active-pane-id 9)
              (ghostel-tmux--wait-for-pane ctl 0))
            (should-not shown)
            (should (equal sent '("select-window -t $1:@0")))
            (should (eq ghostel-tmux--auto-host-window (selected-window))))
        (kill-buffer pane)))))

(ert-deftest ghostel-tmux-test-rows-without-the-current-window-move-no-session ()
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*float-missing*"))
          (sent nil))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--get-or-create-pane)
                     (lambda (_ctl pid _wid _name)
                       (puthash pid pane ghostel-tmux--panes)
                       (with-current-buffer pane
                         (setq-local ghostel-tmux--initialized t))
                       pane)))
            (ghostel-tmux-test--with-sent ctl sent
              (setq ghostel-tmux--auto-host-window (selected-window)
                    ghostel-tmux--current-window-id 7)
              (ghostel-tmux--receive-window-rows
               "@0\t%0\t0\ta\tb25d,80x24,0,0,0\t"))
            (should-not sent)
            (should ghostel-tmux--auto-host-window))
        (kill-buffer pane)))))

(ert-deftest ghostel-tmux-test-session-of-floating-panes-keeps-the-host-window ()
  (ghostel-tmux-test--with-controller ctl
    (save-window-excursion
      (delete-other-windows)
      (set-window-buffer (selected-window) ctl)
      (let ((sent nil))
        (ghostel-tmux-test--with-sent ctl sent
          (setq ghostel-tmux--auto-host-window (selected-window))
          (ghostel-tmux--receive-window-rows "@1\t%1\t1\tb\t0000,\t" t))
        (should-not sent)
        (should ghostel-tmux--auto-host-window)))))

(ert-deftest ghostel-tmux-test-session-of-floating-panes-frees-a-users-window ()
  ;; The window of the session's last tile went on to another buffer.
  (ghostel-tmux-test--with-controller ctl
    (with-temp-buffer
      (let ((other (current-buffer)))
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) other)
          (with-current-buffer ctl
            (setq ghostel-tmux--auto-host-window (selected-window))
            (ghostel-tmux--receive-window-rows "@1\t%1\t1\tb\t0000,\t" t)
            (should-not ghostel-tmux--auto-host-window)))))))

(ert-deftest ghostel-tmux-test-rows-before-the-seed-move-no-session ()
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*float-early*"))
          (sent nil))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--get-or-create-pane)
                     (lambda (_ctl pid _wid _name)
                       (puthash pid pane ghostel-tmux--panes)
                       (with-current-buffer pane
                         (setq-local ghostel-tmux--initialized t))
                       pane)))
            (ghostel-tmux-test--with-sent ctl sent
              (setq ghostel-tmux--auto-host-window (selected-window))
              (ghostel-tmux--receive-window-rows
               "@0\t%0\t0\ta\tb25d,80x24,0,0,0\t"))
            (should-not sent)
            (should ghostel-tmux--auto-host-window))
        (kill-buffer pane)))))

(ert-deftest ghostel-tmux-test-shown-tile-under-a-live-active-pane-keeps-no-window ()
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-floating-active-pane
      (let ((active (generate-new-buffer "*float-live*")))
        (unwind-protect
            (save-window-excursion
              (delete-other-windows)
              (set-window-buffer (selected-window) tiled)
              (with-current-buffer tiled
                (setq-local ghostel-tmux--window-id 0))
              (puthash 9 active ghostel-tmux--panes)
              (setq ghostel-tmux--active-pane-id 9)
              (ghostel-tmux--remove-pane ctl 5)
              (should-not ghostel-tmux--auto-host-window))
          (kill-buffer active))))))

(ert-deftest ghostel-tmux-test-shown-tile-without-an-active-pane-keeps-no-window ()
  ;; As in `--wipe-panes', which forgets the active pane first.
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-floating-active-pane
      (save-window-excursion
        (delete-other-windows)
        (set-window-buffer (selected-window) tiled)
        (with-current-buffer tiled
          (setq-local ghostel-tmux--window-id 0))
        (setq ghostel-tmux--active-pane-id nil)
        (ghostel-tmux--remove-pane ctl 5)
        (should-not ghostel-tmux--auto-host-window)))))

(ert-deftest ghostel-tmux-test-rows-of-a-populated-window-leave-the-kept-window ()
  ;; A window with panes settles the kept window in `--on-layout-change'.
  (ghostel-tmux-test--with-controller ctl
    (let ((moving (generate-new-buffer "*float-moved*"))
          (tile (generate-new-buffer "*float-tile*"))
          shown)
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--get-or-create-pane)
                     (lambda (_ctl pid _wid _name)
                       (puthash pid tile ghostel-tmux--panes)
                       (with-current-buffer tile
                         (setq-local ghostel-tmux--initialized t))
                       tile))
                    ((symbol-function 'ghostel-tmux--display-pane-in-window)
                     (lambda (buf _window) (setq shown buf))))
            (puthash 7 moving ghostel-tmux--panes)
            (setq ghostel-tmux--auto-host-window (selected-window)
                  ghostel-tmux--current-window-id 1)
            (ghostel-tmux--receive-window-rows
             "@1\t%7\t1\tb\tb25d,80x24,0,0,1\t")
            (should-not shown)
            (should ghostel-tmux--auto-host-window))
        (kill-buffer moving)
        (kill-buffer tile)))))

(ert-deftest ghostel-tmux-test-attach-pane-leaves-an-unknown-pane-alone ()
  (ghostel-tmux-test--with-controller ctl
    (ghostel-tmux-test--with-floating-active-pane
      (setq ghostel-tmux--active-pane-id nil)
      (should-not (ghostel-tmux--attach-pane))
      (should-not ghostel-tmux--active-pane-id))))

(ert-deftest ghostel-tmux-test-session-window-changed-other-session ()
  ;; tmux reports this for every session on the server, also for a
  ;; session grouped with ours, whose windows are ours too.
  (ghostel-tmux-test--with-controller ctl
    (let ((followed nil)
          (a (generate-new-buffer "*swc-a*")))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--follow-active-pane)
                     (lambda (pid &rest _) (push pid followed))))
            (puthash 5 a ghostel-tmux--panes)
            (puthash 0 (list :name "w" :panes (list 5) :active-pane 5)
                     ghostel-tmux--windows)
            (setq ghostel-tmux--session-id 1
                  ghostel-tmux--current-window-id 2
                  ghostel-tmux--active-pane-id 7
                  ghostel-tmux--pending-follow-wid 3)
            (ghostel-tmux--on-session-window-changed 9 0)
            (ghostel-tmux--on-session-window-changed 9 40)
            (should (null followed))
            (should (eql ghostel-tmux--current-window-id 2))
            (should (eql ghostel-tmux--active-pane-id 7))
            (should (eql ghostel-tmux--pending-follow-wid 3)))
        (kill-buffer a)))))

(ert-deftest ghostel-tmux-test-follow-leaves-shown-pane-alone ()
  ;; A pane shown in one Emacs window must not take over a second
  ;; window that shows a sibling.
  (ghostel-tmux-test--with-controller ctl
    (let ((displayed nil)
          (a (generate-new-buffer "*fl-a*"))
          (b (generate-new-buffer "*fl-b*")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (set-window-buffer (selected-window) a)
            (set-window-buffer (split-window) b)
            (cl-letf (((symbol-function 'ghostel-tmux--display-pane-in-window)
                       (lambda (buf _w) (push buf displayed))))
              (with-current-buffer ctl
                (puthash 5 a ghostel-tmux--panes)
                (puthash 6 b ghostel-tmux--panes)
                (ghostel-tmux--follow-active-pane 6)))
            (should (null displayed)))
        (kill-buffer a)
        (kill-buffer b)))))

(ert-deftest ghostel-tmux-test-cycle-pane-selects-window ()
  ;; The next pane may be the active pane of another tmux window:
  ;; `select-pane' alone changes nothing there.
  (let ((cmds nil)
        (pane-a (generate-new-buffer "*test-cw-a*"))
        (pane-b (generate-new-buffer "*test-cw-b*")))
    (unwind-protect
        (ghostel-tmux-test--with-controller ctl
          (with-current-buffer pane-b
            (setq-local ghostel-tmux--pane-id 2
                        ghostel-tmux--window-id 7
                        ghostel-tmux--controller-buffer ctl))
          (cl-letf (((symbol-function 'ghostel-tmux--cmd)
                     (lambda (c) (push c cmds)))
                    ((symbol-function 'ghostel-tmux--display-pane-in-window)
                     #'ignore)
                    ((symbol-function 'ghostel-tmux--pane-list)
                     (lambda (_ctl) (list pane-a pane-b))))
            (with-current-buffer pane-a
              (setq-local ghostel-tmux--controller-buffer ctl
                          ghostel-tmux--pane-id 1
                          ghostel-tmux--window-id 0)
              (ghostel-tmux-next-pane)))
          (should (equal (nreverse cmds)
                         '("select-pane -t %2" "select-window -t @7"))))
      (kill-buffer pane-a)
      (kill-buffer pane-b))))

(ert-deftest ghostel-tmux-test-enter-control-mode-renames-host ()
  ;; The host buffer should be hidden from the user-visible buffer
  ;; list while control mode is active (rename to leading space).
  (let ((host (generate-new-buffer "*tmux-test-host-rename*")))
    (unwind-protect
        (progn
          (with-current-buffer host
            (ghostel-mode)
            (setq ghostel-identity '((kind . term))))
          (ghostel-tmux--enter-control-mode host)
          (should (string-prefix-p " *ghostel-tmux-host:" (buffer-name host)))
          (should (equal (buffer-local-value
                          'ghostel-tmux--auto-original-name host)
                         "*tmux-test-host-rename*"))
          ;; `ghostel' must not find the host, nor show it in a window.
          (should-not (buffer-local-value 'ghostel-identity host))
          (with-current-buffer host
            (should (memq #'ghostel-tmux--hide-host
                          window-buffer-change-functions)))
          (ghostel-tmux--exit-control-mode host)
          (should (equal (buffer-name host) "*tmux-test-host-rename*"))
          (should-not (buffer-local-value
                       'ghostel-tmux--auto-original-name host))
          (should (equal (buffer-local-value 'ghostel-identity host)
                         '((kind . term))))
          (with-current-buffer host
            (should-not (memq #'ghostel-tmux--hide-host
                              window-buffer-change-functions))))
      (when (buffer-live-p host) (kill-buffer host)))))

(ert-deftest ghostel-tmux-test-hide-host-shows-previous-buffer ()
  (let ((prev (generate-new-buffer "*tmux-test-prev*"))
        (host (generate-new-buffer " *tmux-test-host*")))
    (unwind-protect
        (save-window-excursion
          (let ((window (selected-window)))
            (set-window-buffer window prev)
            (set-window-buffer window host)
            (ghostel-tmux--hide-host window)
            (should (eq (window-buffer window) host))
            (with-current-buffer host
              (setq-local ghostel-tmux--dcs-active t))
            ;; Before Emacs 31 the hook runs in any buffer.
            (with-temp-buffer
              (ghostel-tmux--hide-host window))
            (should (eq (window-buffer window) prev))
            (should-not (assq host (window-prev-buffers window)))
            ;; Where a pane is to replace it, and in a dedicated window,
            ;; the host stays.
            (set-window-buffer window host)
            (with-current-buffer host
              (setq-local ghostel-tmux--auto-host-window window))
            (ghostel-tmux--hide-host window)
            (should (eq (window-buffer window) host))
            (with-current-buffer host
              (setq-local ghostel-tmux--auto-host-window nil))
            (set-window-dedicated-p window t)
            (unwind-protect
                (ghostel-tmux--hide-host window)
              (set-window-dedicated-p window nil))
            (should (eq (window-buffer window) host))))
      (kill-buffer prev)
      (kill-buffer host))))

(ert-deftest ghostel-tmux-test-hide-host-runs-hooks-of-new-buffer ()
  ;; Only the buffer-local ones: the global ones take a frame.
  (let ((prev (generate-new-buffer "*tmux-test-prev*"))
        (plain (generate-new-buffer "*tmux-test-plain*"))
        (host (generate-new-buffer " *tmux-test-host*"))
        (seen nil))
    (unwind-protect
        (save-window-excursion
          (let ((window (selected-window))
                (window-buffer-change-functions
                 (list (lambda (arg) (push (cons 'global arg) seen)))))
            (with-current-buffer prev
              (add-hook 'window-buffer-change-functions
                        (lambda (w) (push (cons (current-buffer) w) seen))
                        nil t))
            (with-current-buffer host
              (setq-local ghostel-tmux--dcs-active t))
            (set-window-buffer window prev)
            (set-window-buffer window host)
            (ghostel-tmux--hide-host window)
            (should (equal seen (list (cons prev window))))
            (set-window-buffer window plain)
            (set-window-buffer window host)
            (ghostel-tmux--hide-host window)
            (should (eq (window-buffer window) plain))
            (should (equal seen (list (cons prev window))))))
      (kill-buffer prev)
      (kill-buffer plain)
      (kill-buffer host))))

(ert-deftest ghostel-tmux-test-hide-host-with-nothing-to-show ()
  ;; The host keeps the window, and its own hook is not run again.
  (let ((host (generate-new-buffer " *tmux-test-host*"))
        (global nil))
    (unwind-protect
        (save-window-excursion
          (let ((window (selected-window))
                (switch-to-prev-buffer-skip (lambda (&rest _) t))
                (window-buffer-change-functions
                 (list (lambda (arg) (push arg global)))))
            (set-window-buffer window host)
            (with-current-buffer host
              (setq-local ghostel-tmux--dcs-active t)
              (add-hook 'window-buffer-change-functions
                        #'ghostel-tmux--hide-host nil t))
            (ghostel-tmux--hide-host window)
            (should (eq (window-buffer window) host))
            (should-not global)))
      (kill-buffer host))))

(ert-deftest ghostel-tmux-test-exit-control-mode-gives-taken-identity-anew ()
  ;; `ghostel' made a buffer with the host's identity while the host
  ;; had none.
  (let ((host (generate-new-buffer "*tmux-test-host-id*"))
        (other (generate-new-buffer "*tmux-test-other-id*")))
    (unwind-protect
        (progn
          (with-current-buffer host
            (ghostel-mode)
            (setq ghostel-identity '((kind . term) (instance . 1))))
          (ghostel-tmux--enter-control-mode host)
          (with-current-buffer other
            (ghostel-mode)
            (setq ghostel-identity '((kind . term) (instance . 1))))
          (ghostel-tmux--exit-control-mode host)
          (should (equal (buffer-local-value 'ghostel-identity host)
                         '((instance . 2) (kind . term))))
          ;; Without an instance, an identity may be shared.
          (with-current-buffer host
            (setq ghostel-identity '((kind . exec) (command "htop"))))
          (with-current-buffer other
            (setq ghostel-identity '((kind . exec) (command "htop"))))
          (ghostel-tmux--enter-control-mode host)
          (ghostel-tmux--exit-control-mode host)
          (should (equal (buffer-local-value 'ghostel-identity host)
                         '((kind . exec) (command "htop")))))
      (kill-buffer host)
      (kill-buffer other))))

(ert-deftest ghostel-tmux-test-takeover-drops-replies-before-marker ()
  ;; Lines typed into the host before tmux took over get reply blocks
  ;; of their own, before and after `%session-changed'.
  (ghostel-tmux-test--with-host host
    (let ((sent nil)
          (rows nil))
      (cl-letf (((symbol-function 'ghostel-send-string)
                 (lambda (data &optional raw) (should raw) (push data sent)))
                ((symbol-function 'ghostel-tmux--receive-window-rows)
                 (lambda (output _seed) (push output rows))))
        (setq-local ghostel--term 'fake-term)
        (setq ghostel-tmux--dcs-active t)
        (ghostel-tmux--enter-control-mode host)
        (ghostel-tmux-test--feed
         host "%begin 1 1 1\r\nunknown command: ls\r\n%error 1 1 1\r\n")
        (should ghostel-tmux--exit-reason)
        (ghostel-tmux-test--feed host "%session-changed $1 s\r\n")
        (ghostel-tmux--pump-deferred host)
        (should-not ghostel-tmux--exit-reason)
        ;; A line typed without RET joins the spare first line.
        (should (equal sent (list (format "display-message -p ''\n\
display-message -p %s\n"
                                          ghostel-tmux--typed-reply-end))))
        (ghostel-tmux-test--feed
         host "%begin 2 2 1\r\nunknown command: pwd\r\n%error 2 2 1\r\n")
        (ghostel-tmux--pump-deferred host)
        (should (= (length sent) 1))
        (ghostel-tmux-test--feed host ghostel-tmux-test--reply-end)
        (ghostel-tmux--pump-deferred host)
        (should (string-prefix-p "display-message" (car sent)))
        (ghostel-tmux-test--feed host "%begin 3 3 1\r\nh 1 2\r\n%end 3 3 1\r\n")
        (ghostel-tmux--pump-deferred host)
        (should (string-prefix-p "list-windows" (car sent)))
        (ghostel-tmux-test--feed
         host "%begin 4 4 1\r\n@1\t%1\t1\tw\tl\tl\r\n%end 4 4 1\r\n")
        (should (equal rows '("@1\t%1\t1\tw\tl\tl")))))))

(ert-deftest ghostel-tmux-test-send-bytes-from-pane-chunks-large ()
  ;; A paste larger than `--send-keys-chunk-bytes' must be split into
  ;; multiple `send-keys' commands so the controller PTY's input line
  ;; doesn't overflow.  Chunks go through the FIFO command queue: the
  ;; first is sent immediately (nothing in flight), the rest wait for
  ;; their predecessors' reply blocks.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil)
          (big (make-string 1500 ?a)))
      (with-current-buffer ctl
        (setq ghostel-tmux--controller-process 'fake-proc))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (push s sent))))
        (ghostel-tmux--send-bytes-from-pane ctl 0 big)
        (with-current-buffer ctl
          ;; 1500 bytes / 512 per chunk → 3 send-keys commands: one in
          ;; flight, two queued.
          (should (= (length sent) 1))
          (should (= (length ghostel-tmux--command-queue) 2))
          ;; Completing each reply block releases the next chunk.
          (setq ghostel-tmux--parser-state 'idle)
          (ghostel-tmux-test--feed
           ctl "%begin 1 1 1\r\n%end 1 1 1\r\n")
          (ghostel-tmux--pump-command-queue)
          (ghostel-tmux-test--feed
           ctl "%begin 1 2 1\r\n%end 1 2 1\r\n")
          (ghostel-tmux--pump-command-queue)))
      (should (= (length sent) 3))
      (dolist (cmd sent)
        (should (string-prefix-p "send-keys -t %0 -H " cmd))))))

(ert-deftest ghostel-tmux-test-reset-from-auto-takeover-host ()
  ;; `M-x ghostel-tmux-reset' on an auto-takeover host clears
  ;; control state, calls `--exit-control-mode' to seal panes and
  ;; restore the host buffer name, and sends tmux the empty line that
  ;; makes it leave control mode.
  (let ((buf (generate-new-buffer "*reset-host*"))
        exited-buf sent)
    (unwind-protect
        (with-current-buffer buf
          (ghostel-mode)
          (setq ghostel-tmux--dcs-active t
                ghostel-tmux--panes (make-hash-table)
                ghostel--term 'fake-term)
          (cl-letf (((symbol-function 'ghostel-tmux--exit-control-mode)
                     (lambda (b) (setq exited-buf b)))
                    ((symbol-function 'ghostel--on-user-input)
                     (lambda () (ert-fail "counted as typed")))
                    ((symbol-function 'ghostel--write-pty)
                     (lambda (term data) (setq sent (list term data)))))
            (ghostel-tmux-reset))
          (should-not ghostel-tmux--dcs-active)
          (should (eq exited-buf buf))
          (should (equal sent '(fake-term "\n"))))
      (kill-buffer buf))))

(ert-deftest ghostel-tmux-test-takeover-commands-go-out-raw ()
  ;; In the takeover the host's terminal carries the commands; they are
  ;; not typed input, which would scroll the host or end its copy mode.
  (let ((buf (generate-new-buffer "*takeover-host*"))
        sent)
    (unwind-protect
        (with-current-buffer buf
          (ghostel-mode)
          (setq ghostel-tmux--dcs-active t
                ghostel--term 'fake-term)
          (cl-letf (((symbol-function 'ghostel--on-user-input)
                     (lambda () (ert-fail "counted as typed")))
                    ((symbol-function 'ghostel--write-pty)
                     (lambda (term data) (setq sent (list term data)))))
            (ghostel-tmux--controller-send "list-windows\n"))
          (should (equal sent '(fake-term "list-windows\n"))))
      (kill-buffer buf))))

(ert-deftest ghostel-tmux-test-reset-without-session-errors ()
  (with-temp-buffer
    (should-error (ghostel-tmux-reset) :type 'user-error)))

(ert-deftest ghostel-tmux-test-sealed-pane-is-cut-off ()
  ;; A takeover host can attach again; a snapshot of the old attach
  ;; must not act on the new session.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*ghostel-tmux:sealed%0*"))
          (sent nil))
      (unwind-protect
          (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                    ((symbol-function 'process-send-string)
                     (lambda (_p s) (push s sent))))
            (setq ghostel-tmux--controller-process 'fake-proc)
            (with-current-buffer pane
              (ghostel-tmux-pane-mode 1)
              (setq-local ghostel-tmux--controller-buffer ctl
                          ghostel-tmux--pane-id 0))
            (ghostel-tmux--seal-pane-buffer pane)
            (with-current-buffer pane
              (should-not tab-line-mode)
              (should-not (local-variable-p 'tab-line-tabs-function))
              (should-error (ghostel-tmux--prefix-new-window)
                            :type 'user-error))
            (should (null sent)))
        (kill-buffer pane)))))

(ert-deftest ghostel-tmux-test-killed-host-seals-panes ()
  ;; The shell's exit reaches no hook of a buffer that is gone.
  (let ((host (generate-new-buffer "*tmux-host-killed*"))
        (pane (generate-new-buffer "*ghostel-tmux:killed%0*")))
    (unwind-protect
        (progn
          (with-current-buffer pane
            (ghostel-tmux-pane-mode 1)
            (setq ghostel-tmux--controller-buffer host
                  ghostel-pty-out-function #'ignore))
          (with-current-buffer host
            (ghostel-mode)
            (ghostel-tmux--enter-control-mode host)
            (puthash 0 pane ghostel-tmux--panes)
            (setq-local kill-buffer-query-functions nil))
          (kill-buffer host)
          (with-current-buffer pane
            (should-not ghostel-tmux--controller-buffer)
            (should-not ghostel-pty-out-function)
            (should buffer-read-only)
            (should-not tab-line-mode)))
      (when (buffer-live-p host) (kill-buffer host))
      (kill-buffer pane))))

(ert-deftest ghostel-tmux-test-exited-host-drops-kill-hook ()
  (let ((host (generate-new-buffer "*tmux-host-exited*")))
    (unwind-protect
        (with-current-buffer host
          (ghostel-mode)
          (ghostel-tmux--enter-control-mode host)
          (should (memq #'ghostel-tmux--host-killed kill-buffer-hook))
          (ghostel-tmux--exit-control-mode host)
          (should-not (memq #'ghostel-tmux--host-killed kill-buffer-hook)))
      (kill-buffer host))))

(ert-deftest ghostel-tmux-test-capture-has-no-tab-line ()
  ;; A capture has no controller, so no sibling panes to list.
  (let (buf)
    (unwind-protect
        (cl-letf (((symbol-function 'process-file)
                   (lambda (&rest _) (insert "row\n") 0))
                  ((symbol-function 'ghostel-make-terminal)
                   (lambda (name &rest _) (generate-new-buffer name)))
                  ((symbol-function 'ghostel-write-output) #'ignore)
                  ((symbol-function 'pop-to-buffer)
                   (lambda (b) (setq buf b))))
          (ghostel-tmux-capture "s:0.0")
          (with-current-buffer buf
            (should ghostel-tmux-pane-mode)
            (should-not tab-line-mode)
            (should-not (local-variable-p 'tab-line-tabs-function))))
      (when buf (kill-buffer buf)))))

(ert-deftest ghostel-tmux-test-no-command-after-detach ()
  ;; tmux reads nothing after `detach-client'; a command queued behind
  ;; it would reach the host shell as typed input.
  (let ((host (generate-new-buffer "*tmux-host-closing*"))
        (sent nil))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostel-send-string)
                   (lambda (s &optional raw) (should raw) (push s sent))))
          (with-current-buffer host
            (ghostel-mode)
            (setq ghostel-tmux--dcs-active t
                  ghostel-tmux--parser-state 'idle
                  ghostel-tmux--panes (make-hash-table)
                  ghostel--term 'fake-term)
            (ghostel-tmux-detach)
            (ghostel-tmux--queue-command "refresh-client -C 80,24")
            (ghostel-tmux--feed-bytes "%begin 1 1 1\r\n%end 1 1 1\r\n")
            (ghostel-tmux--pump-deferred host))
          (should (equal sent '("detach-client\n"))))
      (kill-buffer host))))

(ert-deftest ghostel-tmux-test-no-command-after-exit ()
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (setq ghostel-tmux--controller-process 'fake-proc
            ghostel-tmux--parser-state 'idle)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (push s sent))))
        (ghostel-tmux--feed-bytes "%exit\r\n")
        (ghostel-tmux--queue-command "list-windows"))
      (should (null sent)))))

(ert-deftest ghostel-tmux-test-exit-drops-queued-commands ()
  ;; A reply and `%exit' in one read: the pump deferred by the reply
  ;; fires after tmux left, and the shell would get the command as input.
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (setq ghostel-tmux--controller-process 'fake-proc
            ghostel-tmux--parser-state 'idle
            ghostel-tmux--command-in-flight (cons "list-windows" nil)
            ghostel-tmux--command-queue (list (cons "refresh-client -C 80,24" nil)))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'process-send-string)
                 (lambda (_p s) (push s sent))))
        (ghostel-tmux--feed-bytes "%begin 1 1 1\r\n%end 1 1 1\r\n%exit\r\n")
        (ghostel-tmux--pump-deferred ctl))
      (should (null sent)))))

(ert-deftest ghostel-tmux-test-enter-control-mode-accepts-commands-again ()
  (ghostel-tmux-test--with-host host
    (setq ghostel-tmux--closing t)
    (ghostel-tmux--enter-control-mode host)
    (should-not ghostel-tmux--closing)))

(ert-deftest ghostel-tmux-test-resize-pane-commits-before-reporting ()
  ;; The terminal clears the prompt row when it commits a resize.  That
  ;; has to happen before tmux hears of the size, or it would clear the
  ;; prompt the shell repaints; `ghostel-set-size' commits at once.
  (let ((ctl (generate-new-buffer "*ghostel-tmux:commit-ctl%0*"))
        (events nil))
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer ctl
            (setq-local ghostel-tmux--controller-process 'fake-proc))
          (setq-local ghostel-tmux--controller-buffer ctl)
          (cl-letf (((symbol-function 'ghostel-set-size)
                     (lambda (&rest _) (push 'resize events)))
                    ((symbol-function 'process-live-p) (lambda (_) t))
                    ((symbol-function 'process-send-string)
                     (lambda (&rest _) (push 'report events))))
            (ghostel-tmux--resize-pane 40 120))
          (should (equal (nreverse events) '(resize report))))
      (kill-buffer ctl))))

(ert-deftest ghostel-tmux-test-resize-pane-passes-an-unchanged-size-on ()
  ;; Every display of a pane comes here.  The size it has still goes to
  ;; the terminal, as a font change can keep the grid but not the cells,
  ;; and to tmux, whose client size may be that of another pane.
  (let ((ctl (generate-new-buffer "*ghostel-tmux:same-ctl%0*"))
        (events nil))
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer ctl
            (setq-local ghostel-tmux--controller-process 'fake-proc))
          (setq-local ghostel-tmux--controller-buffer ctl)
          (cl-letf (((symbol-function 'ghostel-terminal-size)
                     (lambda () '(120 . 40)))
                    ((symbol-function 'ghostel-set-size)
                     (lambda (rows cols) (push (list 'size rows cols) events)))
                    ((symbol-function 'process-live-p) (lambda (_) t))
                    ((symbol-function 'process-send-string)
                     (lambda (&rest _) (push 'report events))))
            (ghostel-tmux--resize-pane 40 120))
          (should (equal (nreverse events) '((size 40 120) report))))
      (kill-buffer ctl))))

(ert-deftest ghostel-tmux-test-set-pane-size-skips-the-size-it-has ()
  ;; A layout names every pane of its window, most of them unchanged.
  (with-temp-buffer
    (let ((sizes nil))
      (cl-letf (((symbol-function 'ghostel-terminal-size)
                 (lambda () '(120 . 40)))
                ((symbol-function 'ghostel-set-size)
                 (lambda (rows cols) (push (list rows cols) sizes))))
        (ghostel-tmux--set-pane-size (current-buffer) 120 40)
        (should-not sizes)
        (ghostel-tmux--set-pane-size (current-buffer) 100 30))
      (should (equal sizes '((30 100)))))))

(ert-deftest ghostel-tmux-test-backfill-fills-the-terminal-rows ()
  ;; The backfill is laid out for the rows the terminal was given.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*ghostel-tmux:rows%0*"))
          (rows nil))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-terminal-size)
                     (lambda () '(100 . 30)))
                    ((symbol-function 'ghostel-tmux--backfill-data)
                     (lambda (_state _primary _rows _screen term-rows)
                       (setq rows term-rows)
                       ""))
                    ((symbol-function 'ghostel-write-output) #'ignore))
            (with-current-buffer ctl
              (puthash 0 pane ghostel-tmux--panes))
            (ghostel-tmux--receive-pane-backfill ctl 0 nil nil "rows" nil)
            (should (eql rows 30)))
        (when (buffer-live-p pane) (kill-buffer pane))))))

(ert-deftest ghostel-tmux-test-session-renamed-other-session ()
  ;; tmux reports this for every session on the server.
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--session-id 0
          ghostel-tmux--session-name "mine")
    (ghostel-tmux--dispatch-notification "%session-renamed $1 other")
    (should (equal ghostel-tmux--session-name "mine"))))

(ert-deftest ghostel-tmux-test-hidden-zoomed-pane-takes-visible-size ()
  ;; The first layout of a zoomed window has its unzoomed geometry; a
  ;; pane that is not on screen has only that one.
  (ghostel-tmux-test--with-controller ctl
    (let ((sizes nil)
          (panes (list (cons 1 (generate-new-buffer "*zoomed*"))
                       (cons 2 (generate-new-buffer "*behind*")))))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--get-or-create-pane)
                     (lambda (_ctl pid _wid _name)
                       (let ((buf (alist-get pid panes)))
                         (with-current-buffer buf
                           (setq-local ghostel--term 'fake
                                       ghostel-tmux--initialized t))
                         (puthash pid buf ghostel-tmux--panes)
                         buf)))
                    ((symbol-function 'ghostel-set-size)
                     (lambda (rows cols)
                       (push (list ghostel-tmux--pane-id cols rows) sizes)))
                    ((symbol-function 'get-buffer-window-list)
                     (lambda (&rest _) nil)))
            (dolist (pane panes)
              (with-current-buffer (cdr pane)
                (setq-local ghostel-tmux--pane-id (car pane))))
            (ghostel-tmux--dispatch-notification
             "%layout-change @0 baaa,120x40,0,0[120x20,0,0,1,120x19,0,21,2] aafe,120x40,0,0,1 -Z")
            (should (equal (nreverse sizes) '((1 120 40) (2 120 19)))))
        (mapc (lambda (pane) (kill-buffer (cdr pane))) panes)))))

(ert-deftest ghostel-tmux-test-active-pane-removal-remembers-window ()
  ;; tmux names the survivor after the pane is gone.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*dying*"))
          (next (generate-new-buffer "*next*")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (set-window-buffer (selected-window) pane)
            (puthash 5 pane ghostel-tmux--panes)
            (puthash 6 next ghostel-tmux--panes)
            (setq ghostel-tmux--active-pane-id 5)
            (ghostel-tmux--remove-pane ctl 5)
            (should (eq ghostel-tmux--auto-host-window (selected-window)))
            (cl-letf (((symbol-function 'ghostel-tmux--display-pane-in-window)
                       (lambda (buf window) (set-window-buffer window buf))))
              (ghostel-tmux--follow-active-pane 6))
            (should (eq (window-buffer) next))
            (should-not ghostel-tmux--auto-host-window))
        (when (buffer-live-p pane) (kill-buffer pane))
        (kill-buffer next)))))

(ert-deftest ghostel-tmux-test-lost-pane-without-report-asks-tmux ()
  ;; No report follows when tmux's active pane already was another one.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*dying*"))
          (next (generate-new-buffer "*next*"))
          (sent nil))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (set-window-buffer (selected-window) pane)
            (ghostel-tmux-test--with-wire ctl sent
              (puthash 5 pane ghostel-tmux--panes)
              (puthash 6 next ghostel-tmux--panes)
              (setq ghostel-tmux--active-pane-id 5)
              (ghostel-tmux--remove-pane ctl 5)
              (should (equal sent '("display-message -p '#{pane_id}'\n")))
              (cl-letf (((symbol-function 'ghostel-tmux--display-pane-in-window)
                         (lambda (buf window) (set-window-buffer window buf))))
                (ghostel-tmux-test--reply ctl "%6\r\n"))
              (should (eq (window-buffer) next))
              (should (eql ghostel-tmux--active-pane-id 6))
              (should-not ghostel-tmux--auto-host-window)))
        (when (buffer-live-p pane) (kill-buffer pane))
        (kill-buffer next)))))

(ert-deftest ghostel-tmux-test-lost-pane-answer-after-report-does-nothing ()
  (ghostel-tmux-test--with-controller ctl
    (cl-letf (((symbol-function 'ghostel-tmux--follow-active-pane)
               (lambda (&rest _) (error "Followed"))))
      (setq ghostel-tmux--active-pane-id 6)
      (ghostel-tmux--receive-active-pane "%7")
      (should (eql ghostel-tmux--active-pane-id 6)))))

(ert-deftest ghostel-tmux-test-lost-pane-answer-without-buffer-waits-for-rows ()
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--auto-host-window (selected-window))
    (ghostel-tmux--receive-active-pane "%7")
    (should ghostel-tmux--auto-host-window)))

(ert-deftest ghostel-tmux-test-lost-pane-failed-query-gives-up ()
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--auto-host-window (selected-window))
    (ghostel-tmux--receive-active-pane nil)
    (should-not ghostel-tmux--auto-host-window)))

(ert-deftest ghostel-tmux-test-session-switch-ignores-active-pane-window ()
  ;; The active pane is shown in another window than the selected one,
  ;; which is the window that stays with the session.
  (ghostel-tmux-test--with-controller ctl
    (let ((a (generate-new-buffer "*sel*"))
          (b (generate-new-buffer "*act*")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (let ((other (split-window)))
              (set-window-buffer (selected-window) a)
              (set-window-buffer other b)
              (puthash 5 a ghostel-tmux--panes)
              (puthash 6 b ghostel-tmux--panes)
              (setq ghostel-tmux--session-id 1
                    ghostel-tmux--active-pane-id 6)
              (ghostel-tmux--on-session-changed 2 "other")
              (should (eq ghostel-tmux--auto-host-window (selected-window)))))
        (when (buffer-live-p a) (kill-buffer a))
        (when (buffer-live-p b) (kill-buffer b))))))

(ert-deftest ghostel-tmux-test-background-pane-removal-keeps-no-window ()
  ;; A later switch must not land in the window of a background pane.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*bg*")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (set-window-buffer (selected-window) pane)
            (puthash 5 pane ghostel-tmux--panes)
            (setq ghostel-tmux--active-pane-id 9)
            (ghostel-tmux--remove-pane ctl 5)
            (should-not ghostel-tmux--auto-host-window))
        (when (buffer-live-p pane) (kill-buffer pane))))))

(ert-deftest ghostel-tmux-test-follow-of-shown-pane-forgets-window ()
  ;; The survivor is already on screen: nothing to redirect, then or later.
  (ghostel-tmux-test--with-controller ctl
    (let ((next (generate-new-buffer "*shown*")))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (let ((other (split-window)))
              (set-window-buffer other next)
              (puthash 6 next ghostel-tmux--panes)
              (setq ghostel-tmux--auto-host-window (selected-window))
              (cl-letf (((symbol-function 'ghostel-tmux--display-pane-in-window)
                         (lambda (&rest _) (error "Displayed"))))
                (ghostel-tmux--follow-active-pane 6))
              (should-not ghostel-tmux--auto-host-window)))
        (kill-buffer next)))))

(ert-deftest ghostel-tmux-test-switch-pane-empty-answer ()
  ;; RET at the prompt answers with the empty string.
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*pick*")))
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) ""))
                    ((symbol-function 'ghostel-tmux--pane-list)
                     (lambda (_) (list pane)))
                    ((symbol-function 'ghostel-tmux--select-pane-buffer)
                     (lambda (_) (error "Selected"))))
            (with-current-buffer pane
              (setq-local ghostel-tmux--controller-buffer ctl)
              (ghostel-tmux-switch-pane)))
        (kill-buffer pane)))))

(ert-deftest ghostel-tmux-test-switch-pane-controller-dies-at-the-prompt ()
  "A controller that goes away leaves the prompt with no choices."
  (let ((ctl (generate-new-buffer " *ctl*")))
    (cl-letf (((symbol-function 'ghostel-tmux--pane-list)
               (lambda (_) (list (current-buffer))))
              ((symbol-function 'completing-read)
               (lambda (_prompt table &rest _)
                 (kill-buffer ctl)
                 (should-not (all-completions "" table))
                 ""))
              ((symbol-function 'ghostel-tmux--select-pane-buffer)
               (lambda (_) (error "Selected"))))
      (with-temp-buffer
        (setq-local ghostel-tmux--controller-buffer ctl)
        (ghostel-tmux-switch-pane)))))

(ert-deftest ghostel-tmux-test-switch-pane-without-panes ()
  ;; A sealed pane has no controller, so nothing to pick from.
  (with-temp-buffer
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (error "Prompted"))))
      (should-error (ghostel-tmux-switch-pane) :type 'user-error))))

(ert-deftest ghostel-tmux-test-capture-feeds-rows-with-cr ()
  ;; capture-pane separates rows with a bare LF, which the VT engine
  ;; renders as a staircase.
  (let ((fed nil) (buf nil))
    (unwind-protect
        (cl-letf (((symbol-function 'process-file)
                   (lambda (&rest _) (insert "one\ntwo\n\n") 0))
                  ((symbol-function 'ghostel-make-terminal)
                   (lambda (name &rest _) (generate-new-buffer name)))
                  ((symbol-function 'ghostel-write-output)
                   (lambda (data &rest _) (setq fed data)))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (b &rest _) (setq buf b))))
          (ghostel-tmux-capture "s:0.0")
          (should (equal fed "one\r\ntwo")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;; ---------------------------------------------------------------------------
;; Tiles
;; ---------------------------------------------------------------------------

(defmacro ghostel-tmux-test--with-tile (ctl pids layout sent &rest body)
  "Run BODY in controller CTL of window @0 with panes PIDS laid out as LAYOUT.
The selected window, alone in the frame, shows the first pane.  SENT
collects the lines sent, newest first.  The panes are plain buffers."
  (declare (indent 4))
  `(ghostel-tmux-test--with-controller ,ctl
     (let ((,sent nil)
           (bufs nil)
           (ghostel-tmux--tiles-exist nil)
           (ghostel-tmux--tile-timer nil)
           (ghostel-tmux--tile-members nil))
       (unwind-protect
           (save-window-excursion
             (delete-other-windows)
             (set-window-parameter (selected-window) 'ghostel-tmux-tile nil)
             (dolist (pid ,pids)
               (let ((buf (generate-new-buffer (format "*ghostel-tmux:s:w%%%d*" pid))))
                 (with-current-buffer buf
                   (setq-local ghostel-tmux--controller-buffer ,ctl)
                   (setq-local ghostel-tmux--pane-id pid)
                   (setq-local ghostel-tmux--window-id 0))
                 (push buf bufs)
                 (puthash pid buf ghostel-tmux--panes)))
             (setq bufs (nreverse bufs))
             (puthash 0 (list :name "w" :panes ,pids
                              :tree (ghostel-tmux--parse-layout-tree ,layout))
                      ghostel-tmux--windows)
             (setq ghostel-tmux--controller-process 'fake-proc
                   ghostel-tmux--parser-state 'idle
                   ghostel-tmux--current-window-id 0
                   ghostel-tmux--active-pane-id (car ,pids))
             (set-window-buffer (selected-window) (car bufs))
             (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                       ((symbol-function 'process-send-string)
                        (lambda (_p s) (push s ,sent)))
                       ((symbol-function 'run-at-time) #'ignore))
               ,@body))
         (mapc #'kill-buffer bufs)))))

(defun ghostel-tmux-test--tile-pids (ctl)
  "Return the panes of the tile of CTL's window @0, top left first."
  (mapcar (lambda (w) (nth 2 (window-parameter w 'ghostel-tmux-tile)))
          (ghostel-tmux--tile-windows ctl 0)))

(ert-deftest ghostel-tmux-test-layout-capacity-counts-separators ()
  "Side by side, tmux puts a column between the parts; stacked, a row."
  (let ((tree (ghostel-tmux--parse-layout-tree
               "abcd,80x24,0,0{40x24,0,0,1,39x24,41,0[39x12,41,0,2,39x11,41,13,3]}")))
    (should (equal (ghostel-tmux--layout-capacity tree #'ignore) '(80 . 24)))))

(ert-deftest ghostel-tmux-test-layout-prune-drops-panes ()
  (let ((tree (ghostel-tmux--parse-layout-tree
               "abcd,80x24,0,0{40x24,0,0,1,39x24,41,0[39x12,41,0,2,39x11,41,13,3]}")))
    (should (equal (ghostel-tmux--layout-prune tree (lambda (pid) (/= pid 2)))
                   '(h 80 24 0 0 ((pane 40 24 0 0 1) (pane 39 11 41 13 3)))))
    (should (equal (ghostel-tmux--layout-prune tree (lambda (pid) (= pid 3)))
                   '(pane 39 11 41 13 3)))
    (should-not (ghostel-tmux--layout-prune tree #'ignore))))

(ert-deftest ghostel-tmux-test-tile-build-splits-to-layout ()
  "A tile shows every pane, each at its tmux width, and selects the active one."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (setq ghostel-tmux--active-pane-id 6)
    (ghostel-tmux--tile-build 0 (selected-window))
    (should (equal (ghostel-tmux-test--tile-pids ctl) '(5 6)))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (should (eq (window-buffer (car windows)) (gethash 5 ghostel-tmux--panes)))
      (should (eq (window-buffer (cadr windows)) (gethash 6 ghostel-tmux--panes)))
      (should (= (window-max-chars-per-line (car windows)) 39))
      (should (eq (selected-window) (cadr windows))))
    (should (equal (plist-get (gethash 0 ghostel-tmux--windows) :tiled) '(5 6)))))

(ert-deftest ghostel-tmux-test-tile-sync-size-reports-the-terminal-area ()
  "The tile reports what its windows show, with tmux's separators."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let* ((windows (ghostel-tmux--tile-windows ctl 0))
           (cols (+ (window-max-chars-per-line (car windows)) 1
                    (window-max-chars-per-line (cadr windows))))
           (rows (floor (with-selected-window (car windows) (window-screen-lines)))))
      (should (ghostel-tmux--tile-sync-size 0))
      (should (equal (car sent) (format "refresh-client -C %d,%d\n" cols rows)))
      ;; The same size is not sent again.
      (setq sent nil)
      (should-not (ghostel-tmux--tile-sync-size 0))
      (should-not sent))))

(ert-deftest ghostel-tmux-test-tile-build-fits-a-small-window ()
  "A layout bigger than the window still shows each pane, and spares a neighbour."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,200x50,0,0{100x50,0,0,5,99x50,101,0,6}" sent
    (let ((foreign (split-window (selected-window) 50 'right)))
      (set-window-buffer foreign (get-buffer-create "*scratch*"))
      (ghostel-tmux--tile-build 0 (selected-window))
      (should (equal (ghostel-tmux-test--tile-pids ctl) '(5 6)))
      (should (= (window-total-width foreign) 30)))))

(ert-deftest ghostel-tmux-test-tile-fit-stacked ()
  "Stacked panes get tmux's rows."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x22,0,0[79x6,0,0,5,79x15,0,7,6]" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((top (car (ghostel-tmux--tile-windows ctl 0))))
      (should (= (floor (with-selected-window top (window-screen-lines))) 6)))))

(ert-deftest ghostel-tmux-test-next-pane-selects-its-tile-window-at-once ()
  "`C-t o' selects the next pane's window before tmux reports the switch."
  (ghostel-tmux-test--with-tile ctl '(5 6 7)
      "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0[39x11,40,0,6,39x11,40,12,7]}" sent
    (setq ghostel-tmux--window-order '(0))
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (select-window (car windows))
      (setq sent nil)
      (with-current-buffer (window-buffer (selected-window))
        (ghostel-tmux--prefix-next-pane))
      (should (eq (selected-window) (nth 1 windows)))
      (should (equal (car (last sent)) "select-pane -t %6\n"))
      (should (eql (buffer-local-value 'ghostel-tmux--active-pane-id ctl) 6))
      ;; tmux's report changes nothing.
      (with-current-buffer ctl
        (ghostel-tmux--on-window-pane-changed 0 6))
      (should (eq (selected-window) (nth 1 windows)))
      ;; The last pane wraps around to the first.
      (select-window (nth 2 windows))
      (with-current-buffer (window-buffer (selected-window))
        (ghostel-tmux--prefix-next-pane))
      (should (eq (selected-window) (car windows))))))

(ert-deftest ghostel-tmux-test-next-pane-ignores-the-report-of-a-replaced-selection ()
  "The report of a selection that a newer one replaces does not move the view."
  (ghostel-tmux-test--with-tile ctl '(5 6 7)
      "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0[39x11,40,0,6,39x11,40,12,7]}" sent
    (setq ghostel-tmux--window-order '(0))
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (select-window (car windows))
      (with-current-buffer (window-buffer (selected-window))
        (ghostel-tmux--prefix-next-pane))
      (with-current-buffer (window-buffer (selected-window))
        (ghostel-tmux--prefix-next-pane))
      (should (eq (selected-window) (nth 2 windows)))
      (with-current-buffer ctl
        (ghostel-tmux--on-window-pane-changed 0 6)
        (should (eql ghostel-tmux--active-pane-id 7)))
      (should (eq (selected-window) (nth 2 windows)))
      ;; Once the selections are answered, a report is followed again.
      (dotimes (_ 4)
        (ghostel-tmux-test--reply ctl))
      (with-current-buffer ctl
        (should-not ghostel-tmux--command-in-flight)
        (ghostel-tmux--on-window-pane-changed 0 7)
        (ghostel-tmux--on-window-pane-changed 0 6))
      (should (eq (selected-window) (nth 1 windows))))))

(ert-deftest ghostel-tmux-test-select-pane-buffer-asks-for-a-pane-recorded-active ()
  "A pane picked is asked for also while Emacs records it as tmux's active one."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    ;; tmux moved on to window @2, whose layout is on its way.
    (setq ghostel-tmux--current-window-id 2
          ghostel-tmux--pending-follow-wid 2)
    (ghostel-tmux--select-pane-buffer (gethash 5 ghostel-tmux--panes))
    (with-current-buffer ctl
      (should (equal (car (last sent)) "select-pane -t %5\n"))
      (should (member "select-window -t @0"
                      (mapcar #'car ghostel-tmux--command-queue)))
      (should (eql ghostel-tmux--current-window-id 0))
      (should-not ghostel-tmux--pending-follow-wid))))

(ert-deftest ghostel-tmux-test-select-pane-buffer-selects-a-zoomed-tile ()
  "A pane that its zoomed tile lacks selects the tile; no other window shows it."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (let ((pane-6 (gethash 6 ghostel-tmux--panes)))
      (ghostel-tmux--tile-build 0 (selected-window))
      (puthash 0 (plist-put (gethash 0 ghostel-tmux--windows) :tree
                            (ghostel-tmux--parse-layout-tree "bbbb,79x23,0,0,5"))
               ghostel-tmux--windows)
      (ghostel-tmux--tile-update 0)
      (should (equal (ghostel-tmux-test--tile-pids ctl) '(5)))
      (let* ((tile (car (ghostel-tmux--tile-windows ctl 0)))
             (other (split-window tile nil 'right)))
        (set-window-buffer other (get-buffer-create "*scratch*"))
        (select-window other)
        (ghostel-tmux--select-pane-buffer pane-6)
        (should (eq (selected-window) tile))
        (should-not (get-buffer-window pane-6))
        (with-current-buffer ctl
          (should (member "select-pane -t %6"
                          (mapcar #'car ghostel-tmux--command-queue))))))))

(ert-deftest ghostel-tmux-test-next-pane-stays-in-its-tmux-window ()
  "`C-t o' cycles the panes of the window shown, not those of others."
  (ghostel-tmux-test--with-controller ctl
    (let ((shown nil)
          (bufs (mapcar (lambda (pid)
                          (let ((buf (generate-new-buffer (format "*next-%d*" pid))))
                            (with-current-buffer buf
                              (setq-local ghostel-tmux--controller-buffer ctl
                                          ghostel-tmux--pane-id pid
                                          ghostel-tmux--window-id (if (= pid 7) 1 0)))
                            (puthash pid buf ghostel-tmux--panes)
                            buf))
                        '(5 6 7))))
      (unwind-protect
          (cl-letf (((symbol-function 'ghostel-tmux--cmd) #'ignore)
                    ((symbol-function 'ghostel-tmux--display-pane-in-window)
                     (lambda (buf _window) (push buf shown))))
            (puthash 0 (list :panes '(5 6)) ghostel-tmux--windows)
            (puthash 1 (list :panes '(7)) ghostel-tmux--windows)
            (setq ghostel-tmux--window-order '(0 1))
            (with-current-buffer (nth 1 bufs)
              (ghostel-tmux--prefix-next-pane))
            (should (equal shown (list (nth 0 bufs)))))
        (mapc #'kill-buffer bufs)))))

(ert-deftest ghostel-tmux-test-tile-check-keeps-a-window-showing-another-buffer ()
  "A tile window showing another buffer keeps its place; nothing is zoomed."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (setq sent nil)
    (set-window-buffer (car (ghostel-tmux--tile-windows ctl 0))
                       (get-buffer-create "*scratch*"))
    (ghostel-tmux--tile-check)
    (should (equal (ghostel-tmux-test--tile-pids ctl) '(5 6)))
    (should-not (cl-find-if (lambda (line) (string-match-p "zoomed" line)) sent))))

(ert-deftest ghostel-tmux-test-tile-check-zooms-a-lone-window ()
  "A tile left with one window of several panes zooms that pane."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (delete-window (car (ghostel-tmux--tile-windows ctl 0)))
    (ghostel-tmux--tile-check)
    (should (equal (car sent) "display-message -p -t %6 '#{window_zoomed_flag}'\n"))))

(ert-deftest ghostel-tmux-test-tile-check-waits-for-the-layout ()
  "A tile whose layout is on its way is not built again."
  (ghostel-tmux-test--with-tile ctl '(5 6 7)
      "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0[39x11,40,0,6,39x11,40,12,7]}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (puthash 0 (plist-put (gethash 0 ghostel-tmux--windows) :settling t)
             ghostel-tmux--windows)
    (delete-window (car (last (ghostel-tmux--tile-windows ctl 0))))
    (ghostel-tmux--tile-check)
    (should (equal (ghostel-tmux-test--tile-pids ctl) '(5 6)))))

(ert-deftest ghostel-tmux-test-tile-collapse-keeps-the-current-buffer ()
  "Deleting the selected window of a tile leaves the current buffer alone."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (select-window (car windows))
      (set-buffer ctl)
      (ghostel-tmux--tile-collapse windows (cadr windows))
      (should (eq (current-buffer) ctl)))))

(ert-deftest ghostel-tmux-test-tile-update-rebuilds-a-new-shape ()
  "A layout of another shape builds the tile again in its space."
  (ghostel-tmux-test--with-tile ctl '(5 6 7) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (puthash 0 (plist-put (gethash 0 ghostel-tmux--windows) :tree
                          (ghostel-tmux--parse-layout-tree
                           "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0[39x11,40,0,6,39x11,40,12,7]}"))
             ghostel-tmux--windows)
    (ghostel-tmux--tile-update 0)
    (should (equal (ghostel-tmux-test--tile-pids ctl) '(5 6 7)))
    (should (= (length (window-list nil 'no-mini)) 3))))

(ert-deftest ghostel-tmux-test-tile-update-zoomed-window-is-one-pane ()
  "The visible layout of a zoomed window collapses the tile to its pane."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (puthash 0 (plist-put (gethash 0 ghostel-tmux--windows) :tree
                          (ghostel-tmux--parse-layout-tree "bbbb,79x23,0,0,6"))
             ghostel-tmux--windows)
    (ghostel-tmux--tile-update 0)
    (should (equal (ghostel-tmux-test--tile-pids ctl) '(6)))
    (should (= (length (window-list nil 'no-mini)) 1))))

(ert-deftest ghostel-tmux-test-tile-update-splits-a-shown-pane ()
  "A split of the window a pane shows builds a tile there."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-update 0)
    (should (equal (ghostel-tmux-test--tile-pids ctl) '(5 6)))))

(ert-deftest ghostel-tmux-test-tile-update-leaves-hidden-window ()
  "A tmux window no Emacs window shows gets no tile."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (set-window-buffer (selected-window) (get-buffer-create "*scratch*"))
    (ghostel-tmux--tile-update 0)
    (should-not (ghostel-tmux--tile-windows ctl 0))
    (should (= (length (window-list nil 'no-mini)) 1))))

(ert-deftest ghostel-tmux-test-tile-check-rebuilds-after-a-deleted-window ()
  "A tile of three that lost a window is built again."
  (ghostel-tmux-test--with-tile ctl '(5 6 7)
      "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0[39x11,40,0,6,39x11,40,12,7]}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (delete-window (car (last (ghostel-tmux--tile-windows ctl 0))))
    (ghostel-tmux--tile-check)
    (should (equal (ghostel-tmux-test--tile-pids ctl) '(5 6 7)))))

(defun ghostel-tmux-test--commands (sent)
  "Return the commands SENT and those still queued, oldest first, as one string."
  (mapconcat #'identity
             (append (reverse sent)
                     (mapcar (lambda (c) (concat (car c) "\n")) ghostel-tmux--command-queue))
             ""))

(ert-deftest ghostel-tmux-test-swap-commands-reorder-the-panes ()
  (should (equal (ghostel-tmux--swap-commands '(5 6 7) '(6 5 7))
                 '("swap-pane -d -s %5 -t %6")))
  (should (equal (ghostel-tmux--swap-commands '(5 6 7) '(6 5 7) 6)
                 '("swap-pane -d -s %6 -t %5")))
  (should (equal (ghostel-tmux--swap-commands '(5 6 7) '(7 5 6))
                 '("swap-pane -d -s %5 -t %7" "swap-pane -d -s %6 -t %5")))
  (should-not (ghostel-tmux--swap-commands '(5 6) '(5 6))))

(ert-deftest ghostel-tmux-test-swapped-window-states-swap-the-panes ()
  "`window-swap-states' in a tile swaps the panes in tmux and keeps the tile."
  (ghostel-tmux-test--with-tile ctl '(5 6 7)
      "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0[39x11,40,0,6,39x11,40,12,7]}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (window-swap-states (nth 0 windows) (nth 1 windows))
      (set-buffer ctl)
      (setq sent nil)
      (ghostel-tmux--tile-check)
      (should (equal (ghostel-tmux--tile-windows ctl 0) windows))
      (should (equal (ghostel-tmux-test--tile-pids ctl) '(6 5 7)))
      (should (eq (window-parameter (nth 2 windows) 'tab-line-format) 'none))
      (should (string-search "swap-pane -d -s %5 -t %6\n" (ghostel-tmux-test--commands sent)))
      ;; Waiting for tmux, a check sends nothing more.
      (let ((before (ghostel-tmux-test--commands sent)))
        (ghostel-tmux--tile-check)
        (should (equal (ghostel-tmux-test--commands sent) before)))
      (ghostel-tmux-test--reply ctl)
      (ghostel-tmux-test--reply
       ctl "aaaa,79x23,0,0{39x23,0,0,6,39x23,40,0[39x11,40,0,5,39x11,40,12,7]}\r\n")
      (should (equal (ghostel-tmux--tile-windows ctl 0) windows))
      (should-not (plist-get (gethash 0 ghostel-tmux--windows) :swap-order)))))

(ert-deftest ghostel-tmux-test-swapped-window-states-of-two-panes-keep-the-tile ()
  "Both windows of a tile of two lose their tile to a swap and get it back."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (setq sent nil)
      (window-swap-states (nth 0 windows) (nth 1 windows))
      (dolist (w windows)
        (ghostel-tmux--apply-pane-size (window-buffer w) w))
      (set-buffer ctl)
      (ghostel-tmux--tile-check)
      (should (equal (ghostel-tmux--tile-windows ctl 0) windows))
      (should (equal (ghostel-tmux-test--tile-pids ctl) '(6 5)))
      (should (string-search "swap-pane -d -s %5 -t %6\n" (ghostel-tmux-test--commands sent))))))

(ert-deftest ghostel-tmux-test-swapped-buffers-swap-the-panes ()
  "Panes swapped between windows of a tile by their buffers swap in tmux."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (set-window-buffer (nth 0 windows) (gethash 6 ghostel-tmux--panes))
      (set-window-buffer (nth 1 windows) (gethash 5 ghostel-tmux--panes))
      (setq sent nil)
      (ghostel-tmux--tile-check)
      (should (equal (ghostel-tmux-test--tile-pids ctl) '(6 5)))
      ;; The selected window's pane, active in tmux, goes first.
      (let ((commands (ghostel-tmux-test--commands sent)))
        (should (string-search "select-pane -t %6\n" commands))
        (should (string-search "swap-pane -d -s %6 -t %5\n" commands))))))

(defun ghostel-tmux-test--reply (ctl &optional payload)
  "Answer CTL's command in flight with PAYLOAD, then send the next one."
  (ghostel-tmux-test--feed ctl (format "%%begin 1 1 1\r\n%s%%end 1 1 1\r\n" (or payload "")))
  (ghostel-tmux--pump-deferred ctl))

(ert-deftest ghostel-tmux-test-swaps-follow-the-swaps-on-their-way ()
  "A swap made before tmux answers the last one swaps from the order it gives."
  (ghostel-tmux-test--with-tile ctl '(5 6 7)
      "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0[39x11,40,0,6,39x11,40,12,7]}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (setq sent nil)
      (window-swap-states (nth 0 windows) (nth 1 windows))
      (set-buffer ctl)
      (ghostel-tmux--tile-check)
      (window-swap-states (nth 1 windows) (nth 2 windows))
      (set-buffer ctl)
      (ghostel-tmux--tile-check)
      (should (equal (ghostel-tmux-test--tile-pids ctl) '(6 7 5)))
      (let ((commands (ghostel-tmux-test--commands sent)))
        (should (string-search "swap-pane -d -s %5 -t %6\n" commands))
        (should (string-search "swap-pane -d -s %5 -t %7\n" commands))
        (should (= 2 (cl-count-if (lambda (c) (string-prefix-p "swap-pane" c))
                                  (split-string commands "\n")))))
      (ghostel-tmux-test--reply ctl)
      (ghostel-tmux-test--reply
       ctl "aaaa,79x23,0,0{39x23,0,0,6,39x23,40,0[39x11,40,0,5,39x11,40,12,7]}\r\n")
      (should (plist-get (gethash 0 ghostel-tmux--windows) :swap-order))
      (should (equal (ghostel-tmux-test--tile-pids ctl) '(6 7 5)))
      (ghostel-tmux-test--reply ctl)
      (ghostel-tmux-test--reply
       ctl "aaaa,79x23,0,0{39x23,0,0,6,39x23,40,0[39x11,40,0,7,39x11,40,12,5]}\r\n")
      (should-not (plist-get (gethash 0 ghostel-tmux--windows) :swap-order))
      (should (equal (ghostel-tmux--tile-windows ctl 0) windows))
      (should (equal (ghostel-tmux-test--tile-pids ctl) '(6 7 5))))))

(ert-deftest ghostel-tmux-test-swapping-back-before-tmux-answers ()
  "Panes swapped back before tmux answers end in the order Emacs shows."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (setq sent nil)
      (dotimes (_ 2)
        (window-swap-states (nth 0 windows) (nth 1 windows))
        (set-buffer ctl)
        (ghostel-tmux--tile-check))
      ;; The active pane is the source of each swap.
      (should (= 2 (cl-count-if (lambda (c) (equal c "swap-pane -d -s %5 -t %6"))
                                (split-string (ghostel-tmux-test--commands sent) "\n"))))
      (ghostel-tmux-test--reply ctl)
      (ghostel-tmux-test--reply ctl "aaaa,79x23,0,0{39x23,0,0,6,39x23,40,0,5}\r\n")
      (ghostel-tmux-test--reply ctl)
      (ghostel-tmux-test--reply ctl "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}\r\n")
      (should (equal (ghostel-tmux--tile-windows ctl 0) windows))
      (should (equal (ghostel-tmux-test--tile-pids ctl) '(5 6))))))

(ert-deftest ghostel-tmux-test-a-swap-while-settling-goes-to-tmux ()
  "Panes swapped while a layout is on its way swap in tmux too."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (puthash 0 (plist-put (gethash 0 ghostel-tmux--windows) :settling t) ghostel-tmux--windows)
      (setq sent nil)
      (window-swap-states (nth 0 windows) (nth 1 windows))
      (set-buffer ctl)
      (ghostel-tmux--tile-check)
      (should (string-search "swap-pane -d -s %5 -t %6\n" (ghostel-tmux-test--commands sent))))))

(ert-deftest ghostel-tmux-test-restore-split-ends-a-swap-on-its-way ()
  "A tile waiting for swaps is built in tmux's order again."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (window-swap-states (nth 0 windows) (nth 1 windows))
      (set-buffer ctl)
      (ghostel-tmux--tile-check)
      (should (plist-get (gethash 0 ghostel-tmux--windows) :swap-order))
      (ghostel-tmux-restore-split)
      (set-buffer ctl)
      (should-not (plist-get (gethash 0 ghostel-tmux--windows) :swap-order))
      (should (equal (ghostel-tmux-test--tile-pids ctl) '(5 6))))))

(ert-deftest ghostel-tmux-test-panes-swapped-in-tmux-move-in-the-tile ()
  "A layout with the panes of a tile in another order shows them so."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (ghostel-tmux--show-layout
     0 (ghostel-tmux--parse-layout-tree "aaaa,79x23,0,0{39x23,0,0,6,39x23,40,0,5}"))
    (should (equal (ghostel-tmux-test--tile-pids ctl) '(6 5)))
    (should (equal (mapcar #'window-buffer (ghostel-tmux--tile-windows ctl 0))
                   (list (gethash 6 ghostel-tmux--panes) (gethash 5 ghostel-tmux--panes))))
    (should-not (string-search "swap-pane" (ghostel-tmux-test--commands sent)))))

(ert-deftest ghostel-tmux-test-a-copied-tile-window-is-part-of-no-tile ()
  "A window state of a tile window put in another window, as by `clone-frame'."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let* ((windows (ghostel-tmux--tile-windows ctl 0))
           (copy (split-window (nth 1 windows) nil 'below)))
      (window-state-put (window-state-get (nth 1 windows)) copy)
      (set-buffer ctl)
      (ghostel-tmux--tile-check)
      (should-not (window-parameter copy 'ghostel-tmux-tile))
      (should (equal (ghostel-tmux-test--tile-pids ctl) '(5 6))))))

(ert-deftest ghostel-tmux-test-restore-split-shows-the-panes-again ()
  "A window of a tile showing another buffer shows its pane again."
  (ghostel-tmux-test--with-tile ctl '(5 6 7)
      "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0[39x11,40,0,6,39x11,40,12,7]}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (select-window (nth 1 windows))
      (set-window-buffer (nth 1 windows) (get-buffer-create "*scratch*"))
      (ghostel-tmux-restore-split)
      (set-buffer ctl)
      (should (equal (ghostel-tmux--tile-windows ctl 0) windows))
      (should (eq (window-buffer (nth 1 windows)) (gethash 6 ghostel-tmux--panes))))))

(ert-deftest ghostel-tmux-test-restore-split-builds-a-lost-tile-again ()
  "A tile that lost windows is built again; a zoomed window is unzoomed."
  (ghostel-tmux-test--with-tile ctl '(5 6 7)
      "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0[39x11,40,0,6,39x11,40,12,7]}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (delete-window (nth 2 (ghostel-tmux--tile-windows ctl 0)))
    (ghostel-tmux-restore-split)
    (should (equal (ghostel-tmux-test--tile-pids ctl) '(5 6 7)))
    (puthash 0 (thread-first (gethash 0 ghostel-tmux--windows)
                             (plist-put :tree (ghostel-tmux--parse-layout-tree "aaaa,79x23,0,0,6"))
                             (plist-put :zoomed t))
             ghostel-tmux--windows)
    (setq sent nil ghostel-tmux--command-queue nil ghostel-tmux--command-in-flight nil)
    (ghostel-tmux-restore-split)
    (should (string-search "resize-pane -Z -t %6\n" (ghostel-tmux-test--commands sent)))))

(ert-deftest ghostel-tmux-test-restore-split-moves-the-tile-to-a-pane-window ()
  "From a window showing a pane outside the tile, the tile moves there."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (let ((other (split-window (selected-window) nil 'below)))
      (ghostel-tmux--tile-build 0 (selected-window))
      (let ((old (ghostel-tmux--tile-windows ctl 0)))
        (set-window-buffer other (gethash 6 ghostel-tmux--panes))
        (select-window other)
        (ghostel-tmux-restore-split)
        (set-buffer ctl)
        (should (memq (selected-window) (ghostel-tmux--tile-windows ctl 0)))
        (should-not (cl-intersection old (ghostel-tmux--tile-windows ctl 0)))
        (should (equal (ghostel-tmux-test--tile-pids ctl) '(5 6)))))))

(ert-deftest ghostel-tmux-test-a-closed-tmux-window-collapses-its-tile ()
  "A tmux window closed while its tile is shown leaves one window."
  (ghostel-tmux-test--with-tile ctl '(5 6 7)
      "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0[39x11,40,0,6,39x11,40,12,7]}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (setq ghostel-tmux--current-window-id 1)
    (ghostel-tmux--drop-window 0)
    (should (= 1 (length (window-list nil 'no-mini))))
    (should-not (window-parameter (selected-window) 'ghostel-tmux-tile))))

(ert-deftest ghostel-tmux-test-a-tab-of-the-selected-tile-keeps-the-selection ()
  "Clicking the tab of the tile the selected window is part of selects nothing."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (select-window (nth 1 windows))
      (set-buffer ctl)
      (setq ghostel-tmux--current-window-id 1)
      (ghostel-tmux--show-tmux-window 0 (nth 0 windows))
      (should (eq (selected-window) (nth 1 windows))))))

(ert-deftest ghostel-tmux-test-turning-splits-off-collapses-the-tiles ()
  "A new `ghostel-tmux-split-windows' applies to the windows shown."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((ghostel-tmux-split-windows nil)
          (zoomed nil))
      (cl-letf (((symbol-function 'ghostel-fit-window) #'ignore)
                ((symbol-function 'ghostel-tmux--ensure-pane-zoomed)
                 (lambda (_wid pid) (push pid zoomed))))
        (ghostel-tmux--split-windows-changed))
      (should (= 1 (length (window-list nil 'no-mini))))
      (should-not (window-parameter (selected-window) 'ghostel-tmux-tile))
      (should zoomed))))

(ert-deftest ghostel-tmux-test-restore-split-refuses-a-detached-pane ()
  (let ((buf (generate-new-buffer "*sealed*")))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer buf
            (setq-local ghostel-tmux-pane-mode t))
          (set-window-buffer (selected-window) buf)
          (should-error (ghostel-tmux-restore-split) :type 'user-error))
      (kill-buffer buf))))

(ert-deftest ghostel-tmux-test-tile-keeps-foreign-window ()
  "A window beside the tile survives building and collapsing it."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,59x23,0,0{29x23,0,0,5,29x23,30,0,6}" sent
    (let ((foreign (split-window (selected-window) 60 'right)))
      (set-window-buffer foreign (get-buffer-create "*scratch*"))
      (ghostel-tmux--tile-build 0 (selected-window))
      (should (= (length (window-list nil 'no-mini)) 3))
      (ghostel-tmux--tile-collapse (ghostel-tmux--tile-windows ctl 0))
      (should (window-live-p foreign))
      (should (= (length (window-list nil 'no-mini)) 2)))))

(ert-deftest ghostel-tmux-test-display-collapses-another-windows-tile ()
  "Showing a pane of another tmux window in a tile window collapses the tile."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((other (generate-new-buffer "*ghostel-tmux:s:x%9*")))
      (unwind-protect
          (progn
            (with-current-buffer other
              (setq-local ghostel-tmux--controller-buffer ctl)
              (setq-local ghostel-tmux--pane-id 9)
              (setq-local ghostel-tmux--window-id 1))
            (puthash 9 other ghostel-tmux--panes)
            (puthash 1 (list :name "x" :panes '(9)
                             :tree (ghostel-tmux--parse-layout-tree "cccc,79x23,0,0,9"))
                     ghostel-tmux--windows)
            (cl-letf (((symbol-function 'ghostel-fit-window) #'ignore))
              (ghostel-tmux--display-pane-in-window other (selected-window)))
            (should-not (ghostel-tmux--tile-windows ctl 0))
            (should (= (length (window-list nil 'no-mini)) 1))
            (should (eq (window-buffer (selected-window)) other)))
        (kill-buffer other)))))

(ert-deftest ghostel-tmux-test-layout-change-of-a-tile-asks-for-the-layout ()
  "A tile follows the visible layout tmux answers, not the one it reported.
tmux resizing a zoomed window reports it unzoomed on the way."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (dolist (pid '(5 6))
      (with-current-buffer (gethash pid ghostel-tmux--panes)
        (setq ghostel-tmux--initialized t)))
    ;; The tile's size is known to tmux already.
    (ghostel-tmux--tile-sync-size 0)
    (ghostel-tmux-test--reply ctl "")
    (setq sent nil)
    (ghostel-tmux-test--feed
     ctl (concat "%layout-change @0 aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}"
                 " aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6} *\r\n"))
    (ghostel-tmux--pump-deferred ctl)
    (should (equal (car sent) "display-message -p -t @0 '#{window_visible_layout}'\n"))
    ;; A second report while the question is out asks nothing more.
    (ghostel-tmux-test--feed
     ctl "%layout-change @0 aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6} bbbb,79x23,0,0,6 *Z\r\n")
    (ghostel-tmux--pump-deferred ctl)
    (should (= (length sent) 1))
    (cl-letf (((symbol-function 'ghostel-set-size) #'ignore))
      (ghostel-tmux-test--reply ctl "bbbb,79x23,0,0,6\r\n"))
    (should (equal (ghostel-tmux-test--tile-pids ctl) '(6)))))

(ert-deftest ghostel-tmux-test-selecting-a-tile-window-selects-its-pane ()
  "The selected window of a tile makes its pane tmux's active one."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (setq sent nil)
    (select-window (cadr (ghostel-tmux--tile-windows ctl 0)))
    (ghostel-tmux--selection-change (selected-frame))
    (ghostel-tmux--pump-deferred ctl)
    (should (equal (car (last sent)) "select-pane -t %6\n"))
    (should (eql (buffer-local-value 'ghostel-tmux--active-pane-id ctl) 6))
    ;; Selecting it again asks nothing.
    (setq sent nil)
    (ghostel-tmux--selection-change (selected-frame))
    (should-not sent)))

(ert-deftest ghostel-tmux-test-reported-pane-selects-its-tile-window ()
  "tmux's active pane moves the selection within the tile."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (should (eq (selected-window) (car windows)))
      (ghostel-tmux-test--feed ctl "%window-pane-changed @0 %6\r\n")
      (should (eq (selected-window) (cadr windows)))
      (should (eq (current-buffer) ctl)))))

(ert-deftest ghostel-tmux-test-reported-pane-leaves-a-window-outside-the-tile ()
  "A report keeps the selection of a window outside the tile."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,59x23,0,0{29x23,0,0,5,29x23,30,0,6}" sent
    (let ((foreign (split-window (selected-window) 60 'right)))
      (set-window-buffer foreign (get-buffer-create "*scratch*"))
      (ghostel-tmux--tile-build 0 (selected-window))
      (select-window foreign)
      (set-buffer ctl)
      (ghostel-tmux-test--feed ctl "%window-pane-changed @0 %6\r\n")
      (should (eq (selected-window) foreign)))))

(ert-deftest ghostel-tmux-test-window-switch-replaces-the-tile ()
  "Switching to another tmux window shows it in the selected tile window."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((other (generate-new-buffer "*ghostel-tmux:s:x%9*"))
          (kept (cadr (ghostel-tmux--tile-windows ctl 0))))
      (unwind-protect
          (progn
            (select-window kept)
            (set-buffer ctl)
            (with-current-buffer other
              (setq-local ghostel-tmux--controller-buffer ctl)
              (setq-local ghostel-tmux--pane-id 9)
              (setq-local ghostel-tmux--window-id 1))
            (puthash 9 other ghostel-tmux--panes)
            (setq ghostel-tmux--session-id 1)
            (puthash 1 (list :name "x" :panes '(9) :active-pane 9
                             :tree (ghostel-tmux--parse-layout-tree "cccc,79x23,0,0,9"))
                     ghostel-tmux--windows)
            (cl-letf (((symbol-function 'ghostel-fit-window) #'ignore))
              (ghostel-tmux-test--feed ctl "%session-window-changed $1 @1\r\n"))
            (should-not (ghostel-tmux--tile-windows ctl 0))
            (should (equal (window-list nil 'no-mini) (list kept)))
            (should (eq (window-buffer kept) other)))
        (kill-buffer other)))))

(ert-deftest ghostel-tmux-test-reported-selection-is-not-sent-back ()
  "A window selected for tmux's report asks tmux for nothing."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (ghostel-tmux--tile-sync-size 0)
    (ghostel-tmux-test--reply ctl "")
    (ghostel-tmux-test--feed ctl "%window-pane-changed @0 %6\r\n")
    (setq sent nil)
    (ghostel-tmux--selection-change (selected-frame))
    (should-not sent)))

(ert-deftest ghostel-tmux-test-report-skips-a-tile-window-showing-another-buffer ()
  "The window of a pane that shows another buffer is not selected."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (set-window-buffer (cadr windows) (get-buffer-create "*scratch*"))
      (ghostel-tmux-test--feed ctl "%window-pane-changed @0 %6\r\n")
      (should (eq (selected-window) (car windows))))))

(ert-deftest ghostel-tmux-test-selecting-a-tile-of-another-window-sends-its-size ()
  "A tile of a tmux window that is not the current one reports its size."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (setq ghostel-tmux--current-window-id 1
          ghostel-tmux--active-pane-id 9
          ghostel-tmux--client-size '(10 . 10)
          sent nil)
    (select-window (cadr (ghostel-tmux--tile-windows ctl 0)))
    (ghostel-tmux--selection-change (selected-frame))
    (ghostel-tmux--pump-deferred ctl)
    (dotimes (_ 2) (ghostel-tmux-test--reply ctl ""))
    (should (equal (mapcar (lambda (line) (car (split-string line " -")))
                           (reverse sent))
                   '("select-pane" "select-window" "refresh-client")))))

(ert-deftest ghostel-tmux-test-window-split-off-a-tile-leaves-it ()
  "A window split off a tile window gets a place of its own."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((foreign (split-window (car (ghostel-tmux--tile-windows ctl 0)) nil 'right)))
      (set-window-buffer foreign (get-buffer-create "*scratch*"))
      (should-not (ghostel-tmux--tile-intact-p 0 (ghostel-tmux--tile-windows ctl 0)))
      (ghostel-tmux--tile-check)
      (let ((windows (ghostel-tmux--tile-windows ctl 0)))
        (should (equal (ghostel-tmux-test--tile-pids ctl) '(5 6)))
        (should (window-live-p foreign))
        (should-not (memq foreign (ghostel-tmux--window-leaves
                                   (ghostel-tmux--tile-root windows))))))))

(ert-deftest ghostel-tmux-test-pane-report-without-tiles-keeps-the-selected-window ()
  "Without splits, a pane change in a tmux window replaces its own pane."
  (let ((ghostel-tmux-split-windows nil))
    (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
      (let ((other (generate-new-buffer "*ghostel-tmux:s:x%9*"))
            (left (selected-window)))
        (unwind-protect
            (let ((right (split-window left nil 'right)))
              (with-current-buffer other
                (setq-local ghostel-tmux--controller-buffer ctl)
                (setq-local ghostel-tmux--pane-id 9)
                (setq-local ghostel-tmux--window-id 1))
              (puthash 9 other ghostel-tmux--panes)
              (set-window-buffer right other)
              (select-window right)
              (set-buffer ctl)
              (cl-letf (((symbol-function 'ghostel-fit-window) #'ignore))
                (ghostel-tmux-test--feed ctl "%window-pane-changed @0 %6\r\n"))
              (should (eq (window-buffer right) other))
              (should (eq (window-buffer left) (gethash 6 ghostel-tmux--panes))))
          (kill-buffer other))))))

(defun ghostel-tmux-test--tab-line-in (window)
  "Return the tab line text WINDOW shows."
  (with-selected-window window
    (with-current-buffer (window-buffer window)
      (substring-no-properties (ghostel-tmux--tab-line)))))

(ert-deftest ghostel-tmux-test-tab-line-has-a-tab-per-tmux-window ()
  "Each tmux window has a tab; the one shown is current."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (puthash 1 (list :name "logs 100%") ghostel-tmux--windows)
    (puthash 0 (plist-put (gethash 0 ghostel-tmux--windows) :name "main")
             ghostel-tmux--windows)
    (setq ghostel-tmux--window-order '(0 1))
    (let ((bar (ghostel-tmux--window-tabs 0 40)))
      (should (string-prefix-p " main   logs 100% " (substring-no-properties bar)))
      (should (= (string-width bar) 40))
      (should (eq (get-text-property 1 'face bar) 'tab-line-tab-current))
      (should (eql (get-text-property 1 'ghostel-tmux-window bar) 0))
      (should (eql (get-text-property 8 'ghostel-tmux-window bar) 1)))))

(ert-deftest ghostel-tmux-test-tab-line-is-one-bar-over-a-tile ()
  "The windows of a tile's top row show their parts of one bar."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (puthash 0 (plist-put (gethash 0 ghostel-tmux--windows) :name (make-string 50 ?a))
             ghostel-tmux--windows)
    (setq ghostel-tmux--window-order '(0))
    (ghostel-tmux--tile-build 0 (selected-window))
    (let* ((windows (ghostel-tmux--tile-windows ctl 0))
           (root (ghostel-tmux--tile-root windows))
           (left (ghostel-tmux-test--tab-line-in (car windows)))
           (right (ghostel-tmux-test--tab-line-in (cadr windows))))
      ;; The border between the two covers a column of the left window.
      (should (= (length left) (ghostel-tmux--tab-line-columns (car windows) root)))
      (should (= (length left) (1- (window-total-width (car windows)))))
      (should (string-prefix-p (make-string (- 51 (length left)) ?a) right))
      (should (string-prefix-p (concat left right)
                               (substring-no-properties
                                (ghostel-tmux--window-tabs 0 (frame-width))))))))

(ert-deftest ghostel-tmux-test-tab-line-only-on-the-top-row ()
  "A window below another in a tile has no tab line; tmux has no row for it."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x22,0,0[79x11,0,0,5,79x10,0,12,6]" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (should-not (window-parameter (car windows) 'tab-line-format))
      (should (eq (window-parameter (cadr windows) 'tab-line-format) 'none))
      (ghostel-tmux--tile-collapse windows (cadr windows))
      (should-not (window-parameter (cadr windows) 'tab-line-format)))))

(ert-deftest ghostel-tmux-test-tab-line-escapes-percent-after-slicing ()
  "A `%' at the seam of two slices stays one character in each."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let* ((windows (ghostel-tmux--tile-windows ctl 0))
           (cols (ghostel-tmux--tab-line-columns
                  (car windows) (ghostel-tmux--tile-root windows))))
      ;; The name puts a `%' in the last column of the left window.
      (puthash 0 (plist-put (gethash 0 ghostel-tmux--windows) :name
                            (concat (make-string (- cols 2) ?a) "%b"))
               ghostel-tmux--windows)
      (setq ghostel-tmux--window-order '(0))
      (let ((left (ghostel-tmux-test--tab-line-in (car windows)))
            (right (ghostel-tmux-test--tab-line-in (cadr windows))))
        (should (string-suffix-p "a%%" left))
        (should (string-prefix-p "b " right))))))

(ert-deftest ghostel-tmux-test-drop-tab-line-clears-the-format ()
  "A sealed pane has no tab line left."
  (with-temp-buffer
    (ghostel-tmux-pane-mode 1)
    (should tab-line-format)
    (ghostel-tmux--drop-tab-line)
    (should-not tab-line-format)
    (ghostel-tmux-pane-mode -1)))

(ert-deftest ghostel-tmux-test-window-tab-of-a-shown-window-selects-it ()
  "A tab of a tmux window shown elsewhere selects its window there."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (setq ghostel-tmux--session-id 2)
    (let* ((foreign (split-window (selected-window) 60 'right))
           (other (generate-new-buffer "*ghostel-tmux:s:x%9*")))
      (unwind-protect
          (progn
            (with-current-buffer other
              (setq-local ghostel-tmux--controller-buffer ctl)
              (setq-local ghostel-tmux--pane-id 9)
              (setq-local ghostel-tmux--window-id 1))
            (puthash 9 other ghostel-tmux--panes)
            (puthash 1 (list :name "x" :panes '(9)) ghostel-tmux--windows)
            (set-window-buffer foreign other)
            (ghostel-tmux--tile-build 0 (selected-window))
            (setq sent nil)
            (ghostel-tmux--show-tmux-window 1 (selected-window))
            (ghostel-tmux--pump-deferred ctl)
            (should (eq (selected-window) foreign))
            (should-not ghostel-tmux--auto-host-window)
            (should (equal sent '("select-window -t $2:@1\n"))))
        (kill-buffer other)))))

(ert-deftest ghostel-tmux-test-failed-window-tab-forgets-the-window ()
  "A tab whose tmux window tmux cannot select leaves no window kept for it."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (cl-letf (((symbol-function 'ghostel-tmux--report-failure) #'ignore))
      (ghostel-tmux--show-tmux-window 7 (selected-window))
      (ghostel-tmux--pump-deferred ctl)
      (should (eq ghostel-tmux--auto-host-window (selected-window)))
      (ghostel-tmux-test--feed
       ctl "%begin 1 1 1\r\ncan't find window\r\n%error 1 1 1\r\n")
      (should-not ghostel-tmux--auto-host-window))))

(ert-deftest ghostel-tmux-test-window-tab-asks-tmux-for-the-window ()
  "A tab of another tmux window selects it, to be shown where it was clicked."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (setq ghostel-tmux--session-id 2)
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((window (cadr (ghostel-tmux--tile-windows ctl 0))))
      (setq sent nil)
      (ghostel-tmux--show-tmux-window 1 window)
      (ghostel-tmux--pump-deferred ctl)
      (should (equal sent '("select-window -t $2:@1\n")))
      (should (eq ghostel-tmux--auto-host-window window))
      ;; The tab of the window shown does nothing.
      (setq sent nil ghostel-tmux--auto-host-window nil)
      (ghostel-tmux--show-tmux-window 0 window)
      (should-not sent)
      (should (equal (ghostel-tmux-test--tile-pids ctl) '(5 6))))))

(defmacro ghostel-tmux-test--with-synced-tile (ctl &rest body)
  "Run BODY after building CTL's tile of @0 and telling tmux its size.
The layout has that size, as tmux's answer to it."
  (declare (indent 1))
  `(progn
     (ghostel-tmux--tile-build 0 (selected-window))
     (ghostel-tmux--tile-sync-size 0)
     (ghostel-tmux-test--reply ,ctl "")
     (let ((tree (plist-get (gethash 0 ghostel-tmux--windows) :tree)))
       (setf (nth 1 tree) (car ghostel-tmux--client-size)
             (nth 2 tree) (cdr ghostel-tmux--client-size)))
     ;; The first check knows the tile's size.
     (ghostel-tmux--tile-check)
     ,@body))

(defun ghostel-tmux-test--resize-commands (ctl)
  "Return the resize commands CTL's tile of @0 asks for."
  (with-current-buffer ctl
    (ghostel-tmux--tile-resize-commands 0 (ghostel-tmux--tile-windows ctl 0))))

(ert-deftest ghostel-tmux-test-dragged-divider-resizes-the-pane ()
  "A window of a tile resized in Emacs asks tmux to resize its pane."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux-test--with-synced-tile ctl
      (let* ((windows (ghostel-tmux--tile-windows ctl 0))
             (left (car windows))
             (scheduled nil))
        (should-not (ghostel-tmux-test--resize-commands ctl))
        (adjust-window-trailing-edge left 5 t)
        (cl-letf (((symbol-function 'run-with-idle-timer)
                   (lambda (&rest args) (setq scheduled args) 'timer))
                  ((symbol-function 'ghostel-tmux--tile-fit)
                   (lambda (&rest _) (error "Fitted"))))
          (ghostel-tmux--tile-check))
        (should (eq (nth 2 scheduled) #'ghostel-tmux--tile-resize))
        (setq sent nil ghostel-tmux--resize-timer nil)
        (ghostel-tmux--tile-resize)
        (ghostel-tmux--pump-deferred ctl)
        (should (equal sent (list (format "resize-pane -t %%5 -x %d\n"
                                          (window-max-chars-per-line left)))))))))

(ert-deftest ghostel-tmux-test-dragged-mode-line-resizes-the-pane-height ()
  "Stacked windows resized in Emacs ask tmux for the height."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x22,0,0[79x11,0,0,5,79x10,0,12,6]" sent
    (ghostel-tmux-test--with-synced-tile ctl
      (let ((top (car (ghostel-tmux--tile-windows ctl 0))))
        (adjust-window-trailing-edge top -3)
        (should (equal (ghostel-tmux-test--resize-commands ctl)
                       (list (format "resize-pane -t %%5 -y %d"
                                     (floor (with-selected-window top
                                              (window-screen-lines)))))))))))

(ert-deftest ghostel-tmux-test-resize-targets-the-part-not-a-pane-inside ()
  "The edge of a column is moved through a pane tmux resizes the column for."
  (ghostel-tmux-test--with-tile ctl '(5 6 7 8)
      "aaaa,79x23,0,0{39x23,0,0[39x11,0,0{19x11,0,0,5,19x11,20,0,6},39x11,0,12,7],39x23,40,0,8}" sent
    (ghostel-tmux-test--with-synced-tile ctl
      (let ((tree (plist-get (gethash 0 ghostel-tmux--windows) :tree)))
        (should (eql (ghostel-tmux--edge-pane (car (nth 5 tree)) t) 7))
        (adjust-window-trailing-edge (alist-get 7 (ghostel-tmux--tile-placed
                                                   (ghostel-tmux--tile-windows ctl 0)))
                                     4 t)
        (should (string-prefix-p "resize-pane -t %7 -x "
                                 (car (ghostel-tmux-test--resize-commands ctl))))))))

(ert-deftest ghostel-tmux-test-resize-without-an-edge-pane-resizes-the-last-part ()
  "A part whose every pane is in a split the same way resizes the last part."
  (ghostel-tmux-test--with-tile ctl '(5 6 7)
      "aaaa,79x23,0,0{49x23,0,0{24x23,0,0,5,24x23,25,0,6},29x23,50,0,7}" sent
    (ghostel-tmux-test--with-synced-tile ctl
      (let ((right (alist-get 7 (ghostel-tmux--tile-placed
                                 (ghostel-tmux--tile-windows ctl 0)))))
        (adjust-window-trailing-edge (alist-get 6 (ghostel-tmux--tile-placed
                                                   (ghostel-tmux--tile-windows ctl 0)))
                                     -4 t)
        (should (equal (car (ghostel-tmux-test--resize-commands ctl))
                       (format "resize-pane -t %%7 -x %d"
                               (window-max-chars-per-line right))))))))

(ert-deftest ghostel-tmux-test-resize-sends-every-changed-edge ()
  "Each part of a split that changed gets its size."
  (ghostel-tmux-test--with-tile ctl '(5 6 7) "aaaa,79x23,0,0{25x23,0,0,5,25x23,26,0,6,27x23,52,0,7}" sent
    (ghostel-tmux-test--with-synced-tile ctl
      (let ((placed (ghostel-tmux--tile-placed (ghostel-tmux--tile-windows ctl 0))))
        (adjust-window-trailing-edge (alist-get 5 placed) 4 t)
        (adjust-window-trailing-edge (alist-get 6 placed) 3 t)
        (should (equal (mapcar (lambda (c) (car (split-string c " -x")))
                               (ghostel-tmux-test--resize-commands ctl))
                       '("resize-pane -t %5" "resize-pane -t %6")))))))

(ert-deftest ghostel-tmux-test-resize-needs-the-tile-to-keep-its-size ()
  "Windows changed with the tile, as by the minibuffer, are fitted back."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x22,0,0[79x11,0,0,5,79x10,0,12,6]" sent
    (ghostel-tmux-test--with-synced-tile ctl
      (let ((top (car (ghostel-tmux--tile-windows ctl 0))))
        (adjust-window-trailing-edge top -3)
        ;; The tile was some other size at the last check.
        (puthash 0 (plist-put (gethash 0 ghostel-tmux--windows) :tile-size '(1 . 1))
                 ghostel-tmux--windows)
        (cl-letf (((symbol-function 'run-with-idle-timer)
                   (lambda (&rest _) (error "Scheduled"))))
          (ghostel-tmux--tile-check))
        (should (= (floor (with-selected-window top (window-screen-lines))) 11))))))

(ert-deftest ghostel-tmux-test-resize-skips-a-window-showing-another-buffer ()
  "A tile with a window showing another buffer resizes no pane."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x22,0,0[79x11,0,0,5,79x10,0,12,6]" sent
    (ghostel-tmux-test--with-synced-tile ctl
      (let ((windows (ghostel-tmux--tile-windows ctl 0)))
        (set-window-buffer (cadr windows) (get-buffer-create "*scratch*"))
        (adjust-window-trailing-edge (car windows) 3)
        (should-not (ghostel-tmux-test--resize-commands ctl))))))

(ert-deftest ghostel-tmux-test-resize-only-for-the-current-window ()
  "A tile of a tmux window that is not the session's current one resizes nothing."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux-test--with-synced-tile ctl
      (adjust-window-trailing-edge (car (ghostel-tmux--tile-windows ctl 0)) 5 t)
      (setq ghostel-tmux--current-window-id 1)
      (should-not (ghostel-tmux-test--resize-commands ctl)))))

(ert-deftest ghostel-tmux-test-resize-needs-a-layout-of-the-tile-size ()
  "Windows that differ from a layout of another size are not a drag.
That layout is from before tmux got the tile's size."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux-test--with-synced-tile ctl
      (adjust-window-trailing-edge (car (ghostel-tmux--tile-windows ctl 0)) 5 t)
      (should (ghostel-tmux-test--resize-commands ctl))
      (setf (nth 1 (plist-get (gethash 0 ghostel-tmux--windows) :tree)) 200)
      (should-not (ghostel-tmux-test--resize-commands ctl)))))

(ert-deftest ghostel-tmux-test-tile-build-shrinks-a-big-layout-in-proportion ()
  "A layout bigger than the window gives each part its share."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,200x23,0,0{100x23,0,0,5,99x23,101,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (should (< (abs (- (window-total-width (car windows))
                         (window-total-width (cadr windows))))
                 3)))))

(ert-deftest ghostel-tmux-test-selection-change-after-a-deleted-window ()
  "The selection hook copes with the window selected before being gone."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((gone (car (ghostel-tmux--tile-windows ctl 0))))
      (select-window gone)
      (delete-window gone)
      (cl-letf (((symbol-function 'frame-old-selected-window) (lambda (&rest _) gone)))
        (ghostel-tmux--selection-change (selected-frame))))))

(ert-deftest ghostel-tmux-test-refused-resize-fits-the-windows-back ()
  "A resize tmux refuses puts the windows back to tmux's sizes."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux-test--with-synced-tile ctl
      (let ((left (car (ghostel-tmux--tile-windows ctl 0))))
        (adjust-window-trailing-edge left 5 t)
        (ghostel-tmux--tile-resize)
        (ghostel-tmux--pump-deferred ctl)
        (ghostel-tmux-test--feed
         ctl "%begin 1 1 1\r\nsize too small\r\n%error 1 1 1\r\n")
        (should (= (window-max-chars-per-line left) 39))))))

(ert-deftest ghostel-tmux-test-resize-waits-for-the-layout ()
  "A drag while a layout is on its way resizes once the layout is there."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux-test--with-synced-tile ctl
      (let ((left (car (ghostel-tmux--tile-windows ctl 0))))
        (adjust-window-trailing-edge left 5 t)
        (puthash 0 (plist-put (gethash 0 ghostel-tmux--windows) :settling t)
                 ghostel-tmux--windows)
        (setq sent nil)
        (cl-letf (((symbol-function 'run-with-idle-timer)
                   (lambda (&rest _) (error "Scheduled"))))
          (ghostel-tmux--tile-resize))
        (should-not sent)
        (cl-letf (((symbol-function 'ghostel-set-size) #'ignore))
          (ghostel-tmux--show-layout
           0 (ghostel-tmux--parse-layout-tree
              (format "aaaa,%dx%d,0,0{39x23,0,0,5,39x23,40,0,6}"
                      (car ghostel-tmux--client-size) (cdr ghostel-tmux--client-size)))))
        (ghostel-tmux--pump-deferred ctl)
        (should (string-prefix-p "resize-pane -t %5 -x " (car (last sent))))
        (should (> (window-max-chars-per-line left) 39))))))

(ert-deftest ghostel-tmux-test-killed-pane-hands-its-tile-window-over ()
  "A tile window of a pane going away shows a sibling, not some other buffer."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((left (car (ghostel-tmux--tile-windows ctl 0)))
          (doomed (gethash 5 ghostel-tmux--panes)))
      (with-current-buffer ctl
        (ghostel-tmux--remove-pane ctl 5 0))
      (should-not (buffer-live-p doomed))
      (should (eq (window-buffer left) (gethash 6 ghostel-tmux--panes))))))

(ert-deftest ghostel-tmux-test-tile-that-lost-a-window-reports-no-size ()
  "A tile that is not as its layout says sends tmux no size."
  (ghostel-tmux-test--with-tile ctl '(5 6 7)
      "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0[39x11,40,0,6,39x11,40,12,7]}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (delete-window (car (last (ghostel-tmux--tile-windows ctl 0))))
    (setq sent nil)
    (should-not (ghostel-tmux--tile-sync-size 0))
    (should-not sent)))

(ert-deftest ghostel-tmux-test-lone-window-showing-another-buffer-zooms-nothing ()
  "The last window of a tile, showing another buffer, leaves the tile alone."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (set-window-buffer (car windows) (get-buffer-create "*scratch*"))
      (delete-window (cadr windows))
      (setq sent nil)
      (ghostel-tmux--tile-check)
      (should-not sent)
      (should (eq (window-buffer (car windows)) (get-buffer "*scratch*")))
      (should-not (window-parameter (car windows) 'ghostel-tmux-tile)))))

(ert-deftest ghostel-tmux-test-leaving-the-minibuffer-keeps-a-selection-made-from-it ()
  "A window selected by a command from the minibuffer stays selected."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (select-window (cadr windows))
      (cl-letf (((symbol-function 'frame-old-selected-window)
                 (lambda (&rest _) (minibuffer-window))))
        (ghostel-tmux--selection-change (selected-frame)))
      (should (eq (selected-window) (cadr windows)))
      (ghostel-tmux--pump-deferred ctl)
      (should (member "select-pane -t %6\n" sent)))))

(ert-deftest ghostel-tmux-test-leaving-the-minibuffer-follows-a-held-report ()
  "A report held back while in the minibuffer is followed on leaving it."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,59x23,0,0{29x23,0,0,5,29x23,30,0,6}" sent
    (let ((mini (split-window (selected-window) 60 'right)))
      (set-window-buffer mini (get-buffer-create "*scratch*"))
      (ghostel-tmux--tile-build 0 (selected-window))
      (let ((windows (ghostel-tmux--tile-windows ctl 0)))
        ;; MINI stands in for the minibuffer window.
        (cl-letf (((symbol-function 'window-minibuffer-p) (lambda (w) (eq w mini))))
          (select-window mini)
          (set-buffer ctl)
          (ghostel-tmux-test--feed ctl "%window-pane-changed @0 %6\r\n")
          (should (eql ghostel-tmux--held-pane 6))
          (should (eq (selected-window) mini))
          (select-window (car windows))
          (cl-letf (((symbol-function 'frame-old-selected-window) (lambda (&rest _) mini)))
            (ghostel-tmux--selection-change (selected-frame))))
        (should (eq (selected-window) (cadr windows)))
        (should-not (buffer-local-value 'ghostel-tmux--held-pane ctl))))))

(ert-deftest ghostel-tmux-test-sealed-panes-collapse-their-tile ()
  "Sealing a session's panes leaves one window of a tile, no longer a tile."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (select-window (nth 1 windows))
      (set-buffer ctl)
      (ghostel-tmux--seal-panes ctl)
      (should (equal (window-list nil 'no-mini) (list (nth 1 windows))))
      (should-not (window-parameter (nth 1 windows) 'ghostel-tmux-tile))
      (should-not (window-parameter (nth 1 windows) 'tab-line-format))
      (with-current-buffer (window-buffer (nth 1 windows))
        (should buffer-read-only)
        (should (equal mode-name "Fundamental:detached"))))))

(ert-deftest ghostel-tmux-test-sealing-keeps-a-tile-window-of-another-buffer ()
  "A window of a tile showing another buffer stays when the panes are sealed."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (set-window-buffer (nth 1 windows) (get-buffer-create "*scratch*"))
      (ghostel-tmux--seal-panes ctl)
      (should (cl-every #'window-live-p windows))
      (should-not (cl-some (lambda (w) (window-parameter w 'ghostel-tmux-tile)) windows)))))

(ert-deftest ghostel-tmux-test-collapse-keeps-the-window-selected-before-the-minibuffer ()
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((windows (ghostel-tmux--tile-windows ctl 0)))
      (select-window (nth 0 windows))
      (cl-letf (((symbol-function 'minibuffer-selected-window) (lambda () (nth 1 windows))))
        (should (eq (ghostel-tmux--tile-collapse (list (nth 1 windows) (nth 0 windows)
                                                       (minibuffer-window)))
                    (nth 0 windows))))
      (should (window-live-p (nth 0 windows))))))

(ert-deftest ghostel-tmux-test-keep-detached-panes ()
  (let ((ghostel-tmux-keep-detached-panes 'ended))
    (should (ghostel-tmux--keep-panes-p nil))
    (should-not (ghostel-tmux--keep-panes-p t)))
  (let ((ghostel-tmux-keep-detached-panes t))
    (should (ghostel-tmux--keep-panes-p t)))
  (let ((ghostel-tmux-keep-detached-panes nil))
    (should-not (ghostel-tmux--keep-panes-p nil))
    (should (ghostel-tmux--keep-panes-p 'keep))))

(ert-deftest ghostel-tmux-test-detach-with-keep-keeps-the-panes ()
  "A detach with KEEP leaves the pane buffers as snapshots."
  (ghostel-tmux-test--with-controller ctl
    (let ((pane (generate-new-buffer "*pane*"))
          (ghostel-tmux-keep-detached-panes 'ended))
      (unwind-protect
          (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                    ((symbol-function 'process-send-string) #'ignore)
                    ((symbol-function 'accept-process-output) #'ignore)
                    ((symbol-function 'delete-process) #'ignore))
            (with-current-buffer pane
              (ghostel-tmux-pane-mode 1)
              (setq ghostel-tmux--controller-buffer ctl
                    ghostel-tmux--pane-id 5))
            (setq ghostel-tmux--controller-process 'fake-proc
                  ghostel-tmux--parser-state 'idle)
            (puthash 5 pane ghostel-tmux--panes)
            (ghostel-tmux-detach t)
            (should (buffer-live-p pane))
            (should-not (buffer-local-value 'ghostel-tmux--controller-buffer pane)))
        (when (buffer-live-p pane) (kill-buffer pane))))))

(ert-deftest ghostel-tmux-test-session-asks-for-the-server-key ()
  "The first session asks for the server's identity before its windows."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux-test--feed ctl "%session-changed $1 s\n")
        (ghostel-tmux--pump-deferred ctl)
        (should (equal (car sent)
                       "display-message -p '#{host} #{pid} #{start_time}'\n"))
        (ghostel-tmux-test--reply ctl "host 42 1700000000\r\n")
        (should (equal ghostel-tmux--server-key "host 42 1700000000"))
        (should (string-prefix-p "list-windows" (car sent)))))))

(ert-deftest ghostel-tmux-test-blank-server-key-is-none ()
  "An empty answer gives no server key, which no sealed pane matches."
  (ghostel-tmux-test--with-controller ctl
    (let ((sent nil))
      (ghostel-tmux-test--with-wire ctl sent
        (ghostel-tmux-test--feed ctl "%session-changed $1 s\n")
        (ghostel-tmux--pump-deferred ctl)
        (ghostel-tmux-test--reply ctl "  \r\n")
        (should-not ghostel-tmux--server-key)))))

(defmacro ghostel-tmux-test--with-snapshot (spec &rest body)
  "Run BODY with a sealed pane buffer of pane 5 bound per SPEC.
SPEC is (VAR SERVER-KEY CONTROLLER).  `ghostel-make-terminal' returns a
buffer it is given."
  (declare (indent 1))
  (pcase-let ((`(,var ,key ,controller) spec))
    `(let ((,var (generate-new-buffer "*ghostel-tmux:s:old%5*")))
       (unwind-protect
           (cl-letf (((symbol-function 'ghostel-make-terminal)
                      (lambda (b &rest _)
                        (if (bufferp b) b (generate-new-buffer b)))))
             (with-current-buffer ,var
               (ghostel-tmux-pane-mode 1)
               (setq ghostel-tmux--pane-id 5
                     ghostel-tmux--server-key ,key
                     ghostel-tmux--initialized t
                     mode-name "Ghostel:detached"))
             (ghostel-tmux--seal-pane-buffer ,var)
             (with-current-buffer ,var
               (setq ghostel-tmux--controller-buffer ,controller))
             ,@body)
         (when (buffer-live-p ,var) (kill-buffer ,var))))))

(ert-deftest ghostel-tmux-test-pane-takes-its-sealed-buffer ()
  "A pane of the server a sealed buffer of it came from goes live in it."
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--server-key "h 1 2"
          ghostel-tmux--session-name "s")
    (ghostel-tmux-test--with-snapshot (old "h 1 2" nil)
      (let ((buf (ghostel-tmux--get-or-create-pane ctl 5 0 "w")))
        (should (eq buf old))
        (should (equal (buffer-name buf) "*ghostel-tmux:s:w%5*"))
        (with-current-buffer buf
          (should (eq ghostel-tmux--controller-buffer ctl))
          (should (equal ghostel-tmux--server-key "h 1 2"))
          (should-not ghostel-tmux--initialized)
          (should (equal mode-name "Ghostel"))
          (should ghostel-pty-out-function))))))

(ert-deftest ghostel-tmux-test-shown-sealed-pane-keeps-its-window ()
  "A sealed pane shown when the session comes back is not replaced by the active one."
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--server-key "h 1 2")
    (ghostel-tmux-test--with-snapshot (old "h 1 2" nil)
      (let ((ghostel-tmux-split-windows nil))
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) old)
          (cl-letf (((symbol-function 'ghostel-tmux--set-pane-size) #'ignore)
                    ((symbol-function 'ghostel-tmux--bootstrap-pane) #'ignore))
            (unwind-protect
                (progn
                  (ghostel-tmux--receive-window-rows
                   "@0\t%6\t1\tw\taaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}\taaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}"
                   t)
                  (should (eq (gethash 5 ghostel-tmux--panes) old))
                  (should (eq (window-buffer (selected-window)) old))
                  (should (eql ghostel-tmux--active-pane-id 6)))
              (when-let* ((b (gethash 6 ghostel-tmux--panes)))
                (kill-buffer b)))))))))

(ert-deftest ghostel-tmux-test-pane-of-another-server-is-new ()
  "A sealed buffer of the same pane id on another server stays."
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--server-key "h 1 3")
    (ghostel-tmux-test--with-snapshot (old "h 1 2" nil)
      (let ((buf (ghostel-tmux--get-or-create-pane ctl 5 0 "w")))
        (unwind-protect
            (progn
              (should-not (eq buf old))
              (should-not (buffer-local-value 'ghostel-tmux--controller-buffer old)))
          (kill-buffer buf))))))

(ert-deftest ghostel-tmux-test-pane-of-a-live-controller-is-not-taken ()
  "The buffer of the pane in another attach of the session stays there."
  (ghostel-tmux-test--with-controller ctl
    (setq ghostel-tmux--server-key "h 1 2")
    (let ((other (generate-new-buffer " *other-ctl*")))
      (unwind-protect
          (ghostel-tmux-test--with-snapshot (old "h 1 2" other)
            (let ((buf (ghostel-tmux--get-or-create-pane ctl 5 0 "w")))
              (unwind-protect
                  (should-not (eq buf old))
                (kill-buffer buf))))
        (kill-buffer other)))))

(ert-deftest ghostel-tmux-test-panes-of-a-session-that-goes-on-are-killed ()
  "After a detach, the pane buffers go, collapsing their tile first."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (let ((bufs (list (gethash 5 ghostel-tmux--panes) (gethash 6 ghostel-tmux--panes)))
          (ghostel-tmux-keep-detached-panes 'ended))
      (setq sent nil)
      (ghostel-tmux--seal-panes ctl t)
      (should-not (cl-some #'buffer-live-p bufs))
      (should (= 1 (length (window-list nil 'no-mini))))
      (should-not sent))))

(ert-deftest ghostel-tmux-test-failed-layout-query-drops-a-pending-resize ()
  "A resize waiting for a layout that does not come is dropped."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (ghostel-tmux--settle-layout 0)
    (puthash 0 (plist-put (gethash 0 ghostel-tmux--windows) :resize-pending t)
             ghostel-tmux--windows)
    (ghostel-tmux--pump-deferred ctl)
    (ghostel-tmux-test--feed ctl "%begin 1 1 1\r\nno window\r\n%error 1 1 1\r\n")
    (should-not (plist-get (gethash 0 ghostel-tmux--windows) :settling))
    (should-not (plist-get (gethash 0 ghostel-tmux--windows) :resize-pending))))

(ert-deftest ghostel-tmux-test-resize-pane-in-a-tile-keeps-tmux-size ()
  "A pane of a tile keeps its size when its window changes."
  (ghostel-tmux-test--with-tile ctl '(5 6) "aaaa,79x23,0,0{39x23,0,0,5,39x23,40,0,6}" sent
    (ghostel-tmux--tile-build 0 (selected-window))
    (setq sent nil)
    (with-current-buffer (gethash 5 ghostel-tmux--panes)
      (setq-local ghostel--term 'fake-term)
      (setq-local ghostel--term-rows 23)
      (setq-local ghostel--term-cols 39)
      (let (sizes)
        (cl-letf (((symbol-function 'ghostel-set-size)
                   (lambda (rows cols) (push (cons cols rows) sizes))))
          (ghostel-tmux--resize-pane 10 10))
        (should (equal sizes '((39 . 23))))))
    (should-not sent)))

(provide 'ghostel-tmux-test)

;;; ghostel-tmux-test.el ends here
