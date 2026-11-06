# `tmux' wrapper for `ghostel-tmux-takeover-mode': runs tmux in control
# mode (-CC) for the subcommands that attach.  Source it in ghostel only:
#   string match -q 'ghostel*' "$INSIDE_EMACS"; and source /path/to/ghostel-tmux/shell/ghostel-tmux.fish
# Bypass with `command tmux ...' or by setting GHOSTEL_TMUX_NO_CC.
if test -z "$GHOSTEL_TMUX_NO_CC"; and not functions -q tmux
    function tmux
        for a in $argv
            if test "$a" = "-C"; or test "$a" = "-CC"
                command tmux $argv
                return $status
            end
        end
        if test (count $argv) -eq 0
            command tmux -CC
            return $status
        end
        set -l _subcmd ""
        set -l _skip 0
        for a in $argv
            if test $_skip -eq 1
                set _skip 0
                continue
            end
            switch $a
                case '-c' '-f' '-L' '-S' '-T'
                    set _skip 1
                case '-*'
                case '*'
                    set _subcmd $a
                    break
            end
        end
        switch $_subcmd
            case "" new new-session attach attach-session a
                command tmux -CC $argv
            case '*'
                command tmux $argv
        end
    end
end
