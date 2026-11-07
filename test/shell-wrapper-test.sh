#!/usr/bin/env bash
# Tests for the `tmux' wrapper in shell/ghostel-tmux.bash and
# shell/ghostel-tmux.nu (the nushell cases are skipped when `nu' is not
# installed).  Verifies that `-CC' is auto-injected for attaching
# subcommands and pass-through for everything else, including hairy
# flag-with-value combinations.
#
# Run: bash test/shell-wrapper-test.sh

set -uo pipefail
unset TMUX

REPO="$(cd "$(dirname "$0")/.." && pwd)"
FAKE_DIR="$(mktemp -d)"
trap 'rm -rf "$FAKE_DIR"' EXIT

# Fake `tmux' that prints its argv to stdout and exits 0, and lists
# $FAKE_PANE_TTY as the only pane, failing without one like a dead server.
# Fake `tty' names the shell's terminal.
cat > "$FAKE_DIR/tmux" <<'EOF'
#!/bin/sh
case "$1" in
    list-panes) [ -n "$FAKE_PANE_TTY" ] || exit 1; echo "$FAKE_PANE_TTY"; exit 0 ;;
    load-buffer) [ -t 0 ] || echo "TMUX-CALLED-WITH:load-buffer $(cat)"; exit 0 ;;
esac
echo "TMUX-CALLED-WITH:$*"
EOF
cat > "$FAKE_DIR/tty" <<'EOF'
#!/bin/sh
[ -z "$FAKE_TTY_FAILS" ] || { echo "not a tty"; exit 1; }
echo /dev/fake-tty
EOF
chmod +x "$FAKE_DIR/tmux" "$FAKE_DIR/tty"

# Environment of a shell in a pane, and of one that only inherited $TMUX.
IN_PANE="TMUX=/tmp/sock,1,0 FAKE_PANE_TTY=/dev/fake-tty"
INHERITED="TMUX=/tmp/sock,1,0 FAKE_PANE_TTY=/dev/other-tty"

PASS=0
FAIL=0

run_one() {
    local label="$1" expected="$2" args="$3" prefix="${4:-}"
    local got
    got=$(PATH="$FAKE_DIR:$PATH" bash --noprofile --norc -c "
        source '$REPO/shell/ghostel-tmux.bash'
        ${prefix}tmux $args
    " 2>&1 | sed -n 's/.*TMUX-CALLED-WITH:\(.*\)/\1/p')
    if [[ "$got" == "$expected" ]]; then
        printf '  PASS  %s\n' "$label"
        PASS=$((PASS + 1))
    else
        printf '  FAIL  %s\n        expected: %q\n        got:      %q\n' \
            "$label" "$expected" "$got"
        FAIL=$((FAIL + 1))
    fi
}

echo "ghostel-tmux shell wrapper tests"
echo

# Bare and attaching subcommands → -CC injected.
run_one "bare tmux"                 "-u -CC"                       ""
run_one "tmux new"                  "-u -CC new"                   "new"
run_one "tmux new -s foo"           "-u -CC new -s foo"            "new -s foo"
run_one "tmux attach"               "-u -CC attach"                "attach"
run_one "tmux attach -t foo"        "-u -CC attach -t foo"         "attach -t foo"
run_one "tmux a -t bar"             "-u -CC a -t bar"              "a -t bar"
run_one "tmux new-session -A -s x"  "-u -CC new-session -A -s x"   "new-session -A -s x"
run_one "tmux attach-session"       "-u -CC attach-session"        "attach-session"

# Already has -CC or -C → don't double up.
run_one "tmux -CC new"              "-CC new"                   "-CC new"
run_one "tmux -C ls"                "-C ls"                     "-C ls"

# Non-attaching subcommands → pass through.
run_one "tmux ls"                   "ls"                        "ls"
run_one "tmux kill-server"          "kill-server"               "kill-server"
run_one "tmux capture-pane -p"      "capture-pane -p"           "capture-pane -p"
run_one "tmux source-file foo"      "source-file foo"           "source-file foo"

# Flags-with-values: -L socket, -S path, -f file, -T features.
run_one "tmux -L sock new"          "-u -CC -L sock new"           "-L sock new"
run_one "tmux -S /tmp/x new -s y"   "-u -CC -S /tmp/x new -s y"    "-S /tmp/x new -s y"
run_one "tmux -L sock ls"           "-L sock ls"                "-L sock ls"
run_one "tmux -f cfg new"           "-u -CC -f cfg new"            "-f cfg new"
run_one "tmux -T 256 new"           "-u -CC -T 256 new"            "-T 256 new"

# Boolean global flags (-N takes no value) must not eat the subcommand.
run_one "tmux -N new"               "-u -CC -N new"                "-N new"
run_one "tmux -N ls"                "-N ls"                     "-N ls"

# Bypass via `command tmux'
got_bypass=$(PATH="$FAKE_DIR:$PATH" bash --noprofile --norc -c "
    source '$REPO/shell/ghostel-tmux.bash'
    command tmux new
" 2>&1 | sed -n 's/.*TMUX-CALLED-WITH:\(.*\)/\1/p')
if [[ "$got_bypass" == "new" ]]; then
    printf '  PASS  %s\n' "command tmux new bypasses wrapper"
    PASS=$((PASS + 1))
else
    printf '  FAIL  %s\n        got: %q\n' \
        "command tmux new bypasses wrapper" "$got_bypass"
    FAIL=$((FAIL + 1))
fi

# A session started detached has no client to take over.  One row per
# case, `|' between what to type and what tmux should get; each shell
# below runs them all.
DETACHED_CASES=(
    "new -d|new -d"
    "new -d -s foo|new -d -s foo"
    "new-session -ds foo|new-session -ds foo"
    "new -dP|new -dP"
    "new -Pd|new -Pd"
    "-L sock new -d|-L sock new -d"
    "new -s d|-u -CC new -s d"
    "new -s -d|-u -CC new -s -d"
    "new -n dev|-u -CC new -n dev"
    "new -e FOO=d|-u -CC new -e FOO=d"
    "new -Asdev|-u -CC new -Asdev"
    "new -Ac/dir|-u -CC new -Ac/dir"
    "new -Ad -s foo|-u -CC new -Ad -s foo"
    "new -d -A|-u -CC new -d -A"
    "new vim -d|-u -CC new vim -d"
    "new -- -d|-u -CC new -- -d"
    "-L -d new|-u -CC -L -d new"
    "attach -d|-u -CC attach -d"
    "-c ls|-c ls"
    "-L sock -c ls|-L sock -c ls"
    "-L -c new|-u -CC -L -c new"
    "new -c /tmp|-u -CC new -c /tmp"
)

for row in "${DETACHED_CASES[@]}"; do
    run_one "tmux ${row%%|*}" "${row##*|}" "${row%%|*}"
done
run_one "tmux new -d \\; attach"    "-u -CC new -d ; attach"    "new -d \\; attach"

# In a pane tmux runs as typed; an inherited $TMUX says nothing.
run_one "tmux new in a pane"        "new"                       "new" "$IN_PANE "
run_one "tmux in a pane"            ""                          "" "$IN_PANE "
run_one "tmux new, inherited TMUX"  "-u -CC new"                "new" "$INHERITED "
run_one "tmux new, dead server"     "-u -CC new"                "new" \
    "TMUX=/tmp/sock,1,0 "
run_one "tmux new, empty TMUX"      "-u -CC new"                "new" "TMUX= "
run_one "tmux new under set -u"     "-u -CC new"                "new" "set -u; "
run_one "tmux new, dead server, set -e" "-u -CC new"            "new" \
    "set -e; TMUX=/tmp/sock,1,0 "
run_one "tmux new, longer pane tty" "-u -CC new"                "new" \
    "TMUX=/tmp/sock,1,0 FAKE_PANE_TTY=/dev/fake-tty1 "
run_one "tmux new, no tty, set -e"  "-u -CC new"                "new" \
    "set -e; FAKE_TTY_FAILS=1 $INHERITED "

check_shell_set_u() {
    local got
    got=$(PATH="$FAKE_DIR:$PATH" bash --noprofile --norc -c "
        set -u
        source '$REPO/shell/ghostel-tmux.bash'
        tmux new
    " 2>&1 | sed -n 's/.*TMUX-CALLED-WITH:\(.*\)/\1/p')
    if [[ "$got" == "-u -CC new" ]]; then
        printf '  PASS  %s\n' "sourced under set -u"
        PASS=$((PASS + 1))
    else
        printf '  FAIL  %s\n        got: %q\n' "sourced under set -u" "$got"
        FAIL=$((FAIL + 1))
    fi
}
check_shell_set_u

# zsh and fish wrappers: the detached cases, skipped without the shell.
check_shell() {
    local label="$1" expected="$2" got="$3"
    if [[ "$got" == "$expected" ]]; then
        printf '  PASS  %s\n' "$label"
        PASS=$((PASS + 1))
    else
        printf '  FAIL  %s\n        expected: %q\n        got:      %q\n' \
            "$label" "$expected" "$got"
        FAIL=$((FAIL + 1))
    fi
}

# An alias of the user's stays, and sourcing the file does not fail.
check_alias() {
    local label="$1" got="$2"
    if [[ "$got" == "OUT:-2 new" ]]; then
        printf '  PASS  %s\n' "$label"
        PASS=$((PASS + 1))
    else
        printf '  FAIL  %s\n        got: %q\n' "$label" "$got"
        FAIL=$((FAIL + 1))
    fi
}
ALIAS_FILTER='s/.*TMUX-CALLED-WITH:/OUT:/'

check_alias "bash: an alias stays" \
    "$(PATH="$FAKE_DIR:$PATH" bash --noprofile --norc -c "
        shopt -s expand_aliases
        alias tmux='tmux -2'
        source '$REPO/shell/ghostel-tmux.bash'
        tmux new
    " 2>&1 | sed "$ALIAS_FILTER")"

if command -v zsh >/dev/null 2>&1; then
    check_alias "zsh: an alias stays" \
        "$(PATH="$FAKE_DIR:$PATH" zsh -f -c "
            alias tmux='tmux -2'
            source '$REPO/shell/ghostel-tmux.zsh'
            eval 'tmux new'
        " 2>&1 | sed "$ALIAS_FILTER")"
    for row in "$IN_PANE|new" "$INHERITED|-u -CC new" \
               "TMUX=x FAKE_PANE_TTY=/dev/fake-tty1|-u -CC new"; do
        check_shell "zsh: ${row%%|*}" "${row##*|}" \
            "$(env PATH="$FAKE_DIR:$PATH" ${row%%|*} zsh -f -c "
                source '$REPO/shell/ghostel-tmux.zsh'
                tmux new
            " 2>&1 | sed -n 's/.*TMUX-CALLED-WITH:\(.*\)/\1/p')"
    done
    check_shell "zsh: dead server under err_exit" "-u -CC new" \
        "$(PATH="$FAKE_DIR:$PATH" TMUX=/tmp/sock,1,0 zsh -f -c "
            source '$REPO/shell/ghostel-tmux.zsh'
            setopt err_exit
            tmux new
        " 2>&1 | sed -n 's/.*TMUX-CALLED-WITH:\(.*\)/\1/p')"
    check_shell "zsh: sourced under no_unset" "-u -CC new" \
        "$(PATH="$FAKE_DIR:$PATH" zsh -f -c "
            setopt no_unset
            source '$REPO/shell/ghostel-tmux.zsh'
            tmux new
        " 2>&1 | sed -n 's/.*TMUX-CALLED-WITH:\(.*\)/\1/p')"
    check_shell "zsh: tmux new under no_unset" "-u -CC new" \
        "$(PATH="$FAKE_DIR:$PATH" zsh -f -c "
            source '$REPO/shell/ghostel-tmux.zsh'
            setopt no_unset
            tmux new
        " 2>&1 | sed -n 's/.*TMUX-CALLED-WITH:\(.*\)/\1/p')"
    check_shell "zsh: sourced under emulate sh" "-u -CC new" \
        "$(PATH="$FAKE_DIR:$PATH" zsh -f -c "
            emulate sh -c \"source '$REPO/shell/ghostel-tmux.zsh'\"
            tmux new
        " 2>&1 | sed -n 's/.*TMUX-CALLED-WITH:\(.*\)/\1/p')"
    echo
    echo "zsh wrapper tests"
    for row in "${DETACHED_CASES[@]}" "new -d \; attach|-u -CC new -d ; attach"; do
        check_shell "zsh: tmux ${row%%|*}" "${row##*|}" \
            "$(PATH="$FAKE_DIR:$PATH" zsh -f -c "
                source '$REPO/shell/ghostel-tmux.zsh'
                tmux ${row%%|*}
            " 2>&1 | sed -n 's/.*TMUX-CALLED-WITH:\(.*\)/\1/p')"
    done
fi

if command -v fish >/dev/null 2>&1; then
    for row in "$IN_PANE|new" "$INHERITED|-u -CC new"; do
        check_shell "fish: ${row%%|*}" "${row##*|}" \
            "$(env PATH="$FAKE_DIR:$PATH" ${row%%|*} fish --no-config -c "
                source '$REPO/shell/ghostel-tmux.fish'
                tmux new
            " 2>&1 | sed -n 's/.*TMUX-CALLED-WITH:\(.*\)/\1/p')"
    done
    echo
    echo "fish wrapper tests"
    for row in "${DETACHED_CASES[@]}" "new -d \; attach|-u -CC new -d ; attach"; do
        check_shell "fish: tmux ${row%%|*}" "${row##*|}" \
            "$(PATH="$FAKE_DIR:$PATH" fish --no-config -c "
                source '$REPO/shell/ghostel-tmux.fish'
                tmux ${row%%|*}
            " 2>&1 | sed -n 's/.*TMUX-CALLED-WITH:\(.*\)/\1/p')"
    done
fi

# --------------------------------------------------------------------------
# Nushell wrapper (shell/ghostel-tmux.nu) — same matrix, skipped without `nu'.
# --------------------------------------------------------------------------

if command -v nu >/dev/null 2>&1; then
    echo
    echo "nushell wrapper tests"
    echo

    run_one_nu() {
        local label="$1" expected="$2" invocation="$3"
        local got
        got=$(env -u GHOSTEL_TMUX_NO_CC INSIDE_EMACS=ghostel PATH="$FAKE_DIR:$PATH" \
            nu --no-config-file -c \
            "source '$REPO/shell/ghostel-tmux.nu'; $invocation" \
            2>&1 </dev/null | sed -n 's/.*TMUX-CALLED-WITH:\(.*\)/\1/p')
        if [[ "$got" == "$expected" ]]; then
            printf '  PASS  %s\n' "$label"
            PASS=$((PASS + 1))
        else
            printf '  FAIL  %s\n        expected: %q\n        got:      %q\n' \
            "$label" "$expected" "$got"
            FAIL=$((FAIL + 1))
        fi
    }

    run_one_nu "nu: bare tmux"           "-u -CC"                    "tmux"
    run_one_nu "nu: tmux new"            "-u -CC new"                "tmux new"
    run_one_nu "nu: tmux attach -t foo"  "-u -CC attach -t foo"      "tmux attach -t foo"
    run_one_nu "nu: tmux a -t bar"       "-u -CC a -t bar"           "tmux a -t bar"
    run_one_nu "nu: tmux -CC new"        "-CC new"                "tmux -CC new"
    run_one_nu "nu: tmux -C ls"          "-C ls"                  "tmux -C ls"
    run_one_nu "nu: tmux ls"             "ls"                     "tmux ls"
    run_one_nu "nu: tmux kill-server"    "kill-server"            "tmux kill-server"
    run_one_nu "nu: tmux -L sock new"    "-u -CC -L sock new"        "tmux -L sock new"
    run_one_nu "nu: tmux -L sock ls"     "-L sock ls"             "tmux -L sock ls"
    run_one_nu "nu: tmux -N ls"          "-N ls"                  "tmux -N ls"
    run_one_nu "nu: tmux -T 256 new"     "-u -CC -T 256 new"         "tmux -T 256 new"
    run_one_nu "nu: ^tmux new bypasses wrapper" "new"             "^tmux new"
    for row in "${DETACHED_CASES[@]}"; do
        run_one_nu "nu: tmux ${row%%|*}" "${row##*|}" "tmux ${row%%|*}"
    done
    run_one_nu "nu: tmux new -d ';' attach" "-u -CC new -d ; attach" "tmux new -d ';' attach"
    run_one_nu "nu: tmux new in a pane" "new" \
        "with-env {TMUX: 'x', FAKE_PANE_TTY: '/dev/fake-tty'} { tmux new }"
    run_one_nu "nu: piped input reaches tmux" "load-buffer x" \
        "'x' | with-env {TMUX: 'x', FAKE_PANE_TTY: '/dev/other-tty', INSIDE_EMACS: ''} { tmux load-buffer - }"
    run_one_nu "nu: tmux new, inherited TMUX" "-u -CC new" \
        "with-env {TMUX: 'x', FAKE_PANE_TTY: '/dev/other-tty'} { tmux new }"

    got_nocc=$(GHOSTEL_TMUX_NO_CC=1 INSIDE_EMACS=ghostel PATH="$FAKE_DIR:$PATH" \
        nu --no-config-file -c \
        "source '$REPO/shell/ghostel-tmux.nu'; tmux new" \
        2>&1 | sed -n 's/.*TMUX-CALLED-WITH:\(.*\)/\1/p')
    if [[ "$got_nocc" == "new" ]]; then
        printf '  PASS  %s\n' "nu: GHOSTEL_TMUX_NO_CC=1 disables wrapper"
        PASS=$((PASS + 1))
    else
        printf '  FAIL  %s\n        got: %q\n' \
            "nu: GHOSTEL_TMUX_NO_CC=1 disables wrapper" "$got_nocc"
        FAIL=$((FAIL + 1))
    fi
else
    echo
    echo "nushell wrapper tests: SKIPPED (no \`nu' in PATH)"
fi

echo
echo "$PASS passed, $FAIL failed"
exit "$FAIL"
