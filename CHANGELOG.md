# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Fixed
- An Emacs input method such as `chinese-py` composes in a pane.
- `C-t o`, `C-t C-n`, `C-t C-p` and `C-t s` select the pane in Emacs at
  once: keys typed before tmux reported the switch went to the pane left.
  In a zoomed split, the pane is still shown once tmux has unzoomed.

### Changed
- Ghostel 0.65.0 or later is required.
- `C-t o` cycles the tiled panes of the window from the one shown, so it
  no longer reaches floating panes (tmux 3.8).

## [0.2.0] — 2026-11-11

### Added
- `ghostel-tmux-new` and `ghostel-tmux-attach` connect to a tmux session
  in control mode (`tmux -CC`) and show each pane as a ghostel buffer,
  backfilled with its scrollback and terminal state.  The panes of a tmux
  window are Emacs window splits, and the tmux windows are tabs in the
  tab line; with `ghostel-tmux-split-windows` nil, a window shows one
  zoomed pane.
- `ghostel-tmux-takeover-mode` turns a `tmux -CC` typed in a ghostel
  shell, also inside ssh, into panes, with the shell's terminal as the
  transport.
- Pane commands under the tmux prefix (`C-t`): next and previous pane,
  a pane picker, `ghostel-tmux-restore-split`, detach, and `C-t :` for
  tmux commands, whose answer is shown.
- `ghostel-tmux-capture` dumps a pane's scrollback into a buffer
  without a connection.
- With a remote `default-directory`, tmux runs on that host through
  TRAMP.
- `ghostel-tmux-reset` recovers from a wedged protocol stream.
- Shell wrappers in `shell/` that add `-CC` to `tmux`.

### Changed
- Requires ghostel 0.64.0, and uses its public functions only.

## [0.1.0] — 2026-11-06

First release.
