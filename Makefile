EMACS      ?= emacs
EMACSFLAGS ?=
ELATE      ?= elate

XDG_CACHE_HOME ?= $(HOME)/.cache
GHOSTEL_DIR    ?= $(firstword $(wildcard $(HOME)/.emacs.d/lib/ghostel) $(XDG_CACHE_HOME)/ghostel)
LINT_ELPA_DIR  ?= $(XDG_CACHE_HOME)/ghostel-tmux-lint-elpa
LINT_DEPS_STAMP := $(LINT_ELPA_DIR)/.deps-installed

LOAD_PATH = -L . -L "$(GHOSTEL_DIR)/lisp"

# ghostel's internal symbols that ghostel-tmux.el may use.
GHOSTEL_INTERNALS =

.PHONY: all byte-compile test test-native test-shell test-elate lint package-lint checkdoc ghostel-internals clean

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

# Needs elate, tmux, the native module built in $(GHOSTEL_DIR), and
# bash, zsh, fish and nushell.
test-elate:
	GHOSTEL_DIR="$(GHOSTEL_DIR)" $(ELATE) matrix --emacs "$$(command -v $(EMACS))" test/elate/takeover.json
	GHOSTEL_DIR="$(GHOSTEL_DIR)" $(ELATE) matrix --emacs "$$(command -v $(EMACS))" test/elate/panes.json
	GHOSTEL_DIR="$(GHOSTEL_DIR)" $(ELATE) matrix --emacs "$$(command -v $(EMACS))" test/elate/subprocess.json
	GHOSTEL_DIR="$(GHOSTEL_DIR)" $(ELATE) matrix --emacs "$$(command -v $(EMACS))" test/elate/splits.json

lint: byte-compile package-lint checkdoc ghostel-internals

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

ghostel-internals: ghostel-tmux.el
	@found=$$(grep -HnoE 'ghostel--[A-Za-z0-9*?/<>=!+_:%&~^-]+' $^); \
	[ $$? -le 1 ] || exit 2; \
	other=$$(printf '%s\n' "$$found" | awk -F: -v allowed='$(GHOSTEL_INTERNALS)' \
	    'BEGIN { split(allowed, a, " "); for (i in a) ok[a[i]] } \
	     $$NF != "" && !($$NF in ok)'); \
	if [ -n "$$other" ]; then \
	  printf '%s\n' "$$other" "ghostel internals in use; see GHOSTEL_INTERNALS"; \
	  exit 1; \
	fi

clean:
	rm -f ghostel-tmux.elc
