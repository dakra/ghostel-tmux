EMACS      ?= emacs
EMACSFLAGS ?=

XDG_CACHE_HOME ?= $(HOME)/.cache
GHOSTEL_DIR    ?= $(firstword $(wildcard $(HOME)/.emacs.d/lib/ghostel) $(XDG_CACHE_HOME)/ghostel)
LINT_ELPA_DIR  ?= $(XDG_CACHE_HOME)/ghostel-tmux-lint-elpa
LINT_DEPS_STAMP := $(LINT_ELPA_DIR)/.deps-installed

LOAD_PATH = -L . -L "$(GHOSTEL_DIR)/lisp"

.PHONY: all byte-compile test test-native test-shell lint package-lint checkdoc clean

all: byte-compile test test-shell lint

$(GHOSTEL_DIR):
	git clone --depth 1 https://github.com/dakra/ghostel.git "$@"

ghostel-tmux.elc: ghostel-tmux.el | $(GHOSTEL_DIR)
	$(EMACS) --batch $(EMACSFLAGS) -Q $(LOAD_PATH) \
		--eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $<

byte-compile: ghostel-tmux.elc

test: ghostel-tmux.elc
	$(EMACS) --batch $(EMACSFLAGS) -Q $(LOAD_PATH) \
		-l ert -l test/ghostel-tmux-test.el -f ert-run-tests-batch-and-exit

# Needs the native module built in $(GHOSTEL_DIR).
test-native: ghostel-tmux.elc
	$(EMACS) --batch $(EMACSFLAGS) -Q $(LOAD_PATH) \
		-l ert -l test/ghostel-tmux-integration-test.el -f ert-run-tests-batch-and-exit

test-shell:
	bash test/shell-wrapper-test.sh

lint: byte-compile package-lint checkdoc

$(LINT_DEPS_STAMP): | $(GHOSTEL_DIR)
	$(EMACS) --batch $(EMACSFLAGS) -Q \
		--eval "(setq package-user-dir \"$(LINT_ELPA_DIR)\")" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(package-install 'package-lint)" \
		--eval "(package-install-file (expand-file-name \"$(GHOSTEL_DIR)/lisp/ghostel.el\"))"
	@touch $@

package-lint: $(LINT_DEPS_STAMP) ghostel-tmux.el
	$(EMACS) --batch $(EMACSFLAGS) -Q \
		--eval "(setq package-user-dir \"$(LINT_ELPA_DIR)\")" \
		--eval "(package-initialize)" \
		--eval "(require 'package-lint)" \
		-f package-lint-batch-and-exit ghostel-tmux.el

checkdoc: ghostel-tmux.el
	$(EMACS) --batch $(EMACSFLAGS) -Q \
		--eval "(require 'checkdoc)" \
		--eval "(let ((sentence-end-double-space nil) \
		              (checkdoc-verb-check-experimental-flag nil) \
		              (ok t)) \
		  (ignore-errors (kill-buffer \"*Warnings*\")) \
		  (let ((inhibit-message t)) (checkdoc-file \"ghostel-tmux.el\")) \
		  (when (get-buffer \"*Warnings*\") \
		    (setq ok nil) \
		    (with-current-buffer \"*Warnings*\" (message \"%s\" (buffer-string)))) \
		  (unless ok (kill-emacs 1)))"

clean:
	rm -f ghostel-tmux.elc
