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
        ^tmux -CC
        return
    }
    # Find the first non-flag argument (the subcommand), skipping
    # values for flags that take arguments.  See tmux(1) §OPTIONS.
    mut subcmd = ""
    mut skip = false
    for arg in $args {
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
    if ($subcmd in ["" "new" "new-session" "attach" "attach-session" "a"]) {
        ^tmux -CC ...$args
    } else {
        ^tmux ...$args
    }
}
