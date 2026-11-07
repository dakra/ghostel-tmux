# `tmux' wrapper for `ghostel-tmux-takeover-mode': runs tmux in control
# mode (-CC) for the subcommands that attach.  Source it in ghostel only:
#   [[ "${INSIDE_EMACS%%,*}" = 'ghostel' ]] && source /path/to/ghostel-tmux/shell/ghostel-tmux.zsh
# Bypass with `command tmux ...' or by exporting GHOSTEL_TMUX_NO_CC=1
# before sourcing this file.
# `function' keeps an alias out of the definition, which is parsed even
# when the alias makes the test below false.
if [[ -z "${GHOSTEL_TMUX_NO_CC:-}" ]] && (( ! ${+functions[tmux]} && ! ${+aliases[tmux]} )); then
    function tmux {
        local _arg
        # In a pane a control client is no use.  `$TMUX' alone does not say
        # so: an Emacs started from tmux passes it on to its shells.
        if [[ -n "${TMUX:-}" ]]; then
            local _tty _ttys
            _tty=$(tty) || true
            _ttys=$(command tmux list-panes -a -F '#{pane_tty}' 2>/dev/null) || true
            if [[ $'\n'"$_ttys"$'\n' == *$'\n'"$_tty"$'\n'* ]]; then
                command tmux "$@"
                return $?
            fi
        fi
        for _arg in "$@"; do
            case "$_arg" in -C|-CC) command tmux "$@"; return $? ;; esac
        done
        if (( $# == 0 )); then
            command tmux -u -CC
            return $?
        fi
        local _subcmd="" _skip=0 _next=1
        for _arg in "$@"; do
            _next=$((_next + 1))
            if (( _skip )); then _skip=0; continue; fi
            case "$_arg" in
                # `-c' runs a shell command instead of a client.
                -c) command tmux "$@"; return $? ;;
                -[fLST]) _skip=1 ;;
                -*) ;;
                *) _subcmd="$_arg"; break ;;
            esac
        done
        # A session started detached has no client to take over, unless
        # `-A' finds it running or a chained command attaches.  Flags end
        # at the first argument that is none, and some take a value.
        case "$_subcmd" in
            new|new-session)
                local _detached=0
                _skip=0
                for _arg in "${@:_next}"; do
                    if (( _skip )); then _skip=0; continue; fi
                    case "$_arg" in -[!-]*) ;; *) break ;; esac
                    if [[ "$_arg" =~ ^-[DEPXd]*A ]]; then _detached=0; break; fi
                    if [[ "$_arg" =~ ^-[DEPX]*d ]]; then _detached=1; fi
                    if [[ "$_arg" =~ ^-[DEPXd]*[cefFnstxy]$ ]]; then _skip=1; fi
                done
                if (( _detached )) && [[ " $* " != *" ; "* ]]; then
                    command tmux "$@"
                    return $?
                fi ;;
        esac
        case "$_subcmd" in
            ""|new|new-session|attach|attach-session|a)
                command tmux -u -CC "$@" ;;
            *)
                command tmux "$@" ;;
        esac
    }
fi
