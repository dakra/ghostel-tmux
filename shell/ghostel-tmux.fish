# `tmux' wrapper for `ghostel-tmux-takeover-mode': runs tmux in control
# mode (-CC) for the subcommands that attach.  Source it in ghostel only:
#   string match -q 'ghostel*' "$INSIDE_EMACS"; and source /path/to/ghostel-tmux/shell/ghostel-tmux.fish
# Bypass with `command tmux ...' or by setting GHOSTEL_TMUX_NO_CC.
if test -z "$GHOSTEL_TMUX_NO_CC"; and not functions -q tmux
    function tmux
        # In a pane a control client is no use.  `$TMUX' alone does not say
        # so: an Emacs started from tmux passes it on to its shells.
        if test -n "$TMUX"
            and contains -- (tty) (command tmux list-panes -a -F '#{pane_tty}' 2>/dev/null)
            command tmux $argv
            return $status
        end
        for a in $argv
            if test "$a" = "-C"; or test "$a" = "-CC"
                command tmux $argv
                return $status
            end
        end
        if test (count $argv) -eq 0
            command tmux -u -CC
            return $status
        end
        set -l _subcmd ""
        set -l _skip 0
        set -l _rest $argv
        for a in $argv
            set -e _rest[1]
            if test $_skip -eq 1
                set _skip 0
                continue
            end
            switch $a
                case '-c'
                    # Runs a shell command instead of a client.
                    command tmux $argv
                    return $status
                case '-f' '-L' '-S' '-T'
                    set _skip 1
                case '-*'
                case '*'
                    set _subcmd $a
                    break
            end
        end
        # A session started detached has no client to take over, unless
        # `-A' finds it running or a chained command attaches.  Flags end
        # at the first argument that is none, and some take a value.
        if contains -- $_subcmd new new-session
            and not contains -- ';' $argv
            set -l _detached 0
            set _skip 0
            for a in $_rest
                if test $_skip -eq 1
                    set _skip 0
                    continue
                end
                string match -qr -- '^-[^-]' $a; or break
                if string match -qr -- '^-[DEPXd]*A' $a
                    set _detached 0
                    break
                end
                string match -qr -- '^-[DEPX]*d' $a; and set _detached 1
                string match -qr -- '^-[DEPXd]*[cefFnstxy]$' $a; and set _skip 1
            end
            if test $_detached -eq 1
                command tmux $argv
                return $status
            end
        end
        switch $_subcmd
            case "" new new-session attach attach-session a
                command tmux -u -CC $argv
            case '*'
                command tmux $argv
        end
    end
end
