# `tmux' wrapper for `ghostel-tmux-takeover-mode': runs tmux in control
# mode (-CC) for the subcommands that attach.  Source it from config.nu:
#   source /path/to/ghostel-tmux/shell/ghostel-tmux.nu
# It only acts inside ghostel; nushell `def' cannot be conditionally
# defined, so that is checked at runtime.  Bypass with `^tmux ...' or by
# setting GHOSTEL_TMUX_NO_CC.
def --wrapped tmux [...args] {
    # Outside ghostel, wrapper disabled, or -C/-CC already passed: run as-is.
    if ((not ($env.INSIDE_EMACS? | default "" | str starts-with "ghostel"))
        or (not ($env.GHOSTEL_TMUX_NO_CC? | is-empty))
        or ("-C" in $args) or ("-CC" in $args)) {
        ^tmux ...$args
        return
    }
    # Bare `tmux' defaults to new-session, which benefits from -CC.
    if ($args | is-empty) {
        ^tmux -u -CC
        return
    }
    # Find the first non-flag argument (the subcommand), skipping
    # values for flags that take arguments.  See tmux(1) §OPTIONS.
    mut subcmd = ""
    mut skip = false
    mut rest = $args
    for arg in $args {
        $rest = ($rest | skip 1)
        if $skip {
            $skip = false
            continue
        }
        if ($arg in ["-c" "-f" "-L" "-S" "-T"]) {
            $skip = true
        } else if ($arg | str starts-with "-") {
            # flag without an argument
        } else {
            $subcmd = $arg
            break
        }
    }
    # A session started detached has no client to take over, unless
    # `-A' finds it running or a chained command attaches.  Flags end at
    # the first argument that is none, and some take a value.
    mut detached = false
    if ($subcmd in ["new" "new-session"]) and (not (";" in $args)) {
        $skip = false
        for arg in $rest {
            if $skip {
                $skip = false
                continue
            }
            if not ($arg =~ '^-[^-]') {
                break
            }
            if $arg =~ '^-[DEPXd]*A' {
                $detached = false
                break
            }
            if $arg =~ '^-[DEPX]*d' {
                $detached = true
            }
            if $arg =~ '^-[DEPXd]*[cefFnstxy]$' {
                $skip = true
            }
        }
    }
    if ($subcmd in ["" "new" "new-session" "attach" "attach-session" "a"]) and (not $detached) {
        ^tmux -u -CC ...$args
    } else {
        ^tmux ...$args
    }
}
