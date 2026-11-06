# `tmux' wrapper for `ghostel-tmux-takeover-mode': runs tmux in control
# mode (-CC) for the subcommands that attach.  Source it in ghostel only:
#   [[ "${INSIDE_EMACS%%,*}" = 'ghostel' ]] && source /path/to/ghostel-tmux/shell/ghostel-tmux.zsh
# Bypass with `command tmux ...' or by exporting GHOSTEL_TMUX_NO_CC=1
# before sourcing this file.
if [[ -z "$GHOSTEL_TMUX_NO_CC" ]] && (( ! $+functions[tmux] )); then
    tmux() {
        local _arg
        for _arg in "$@"; do
            case "$_arg" in -C|-CC) command tmux "$@"; return $? ;; esac
        done
        if (( $# == 0 )); then
            command tmux -CC
            return $?
        fi
        local _subcmd="" _skip=0
        for _arg in "$@"; do
            if (( _skip )); then _skip=0; continue; fi
            case "$_arg" in
                -[cfLST]) _skip=1 ;;
                -*) ;;
                *) _subcmd="$_arg"; break ;;
            esac
        done
        case "$_subcmd" in
            ""|new|new-session|attach|attach-session|a)
                command tmux -CC "$@" ;;
            *)
                command tmux "$@" ;;
        esac
    }
fi
