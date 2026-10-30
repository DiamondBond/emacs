# Emacs config

Personal Emacs 31 config. Emacs sits next to Claude Code here: scratch notes,
todo/plan markdown, magit, vterm, quick edits.

## Layout
- `early-init.el`: startup tuning (GC held off until gcmh takes over; no package.el)
- `init.el`: straight.el bootstrap, the `globals--*` block, then `(require ...)` of each `lisp/` module
- `lisp/base.el` built-in settings · `lisp/keybinds.el` the `C-1` prefix (`z-map`) and global keys ·
  `lisp/packages.el` packages · `lisp/languages.el` modes, treesit, eglot · `lisp/functions.el` personal commands
- `custom.el` is written by Custom

## Conventions
- Tabs for indentation, matching the existing files.
- One `use-package` block per package, under a short `;; Description` comment.
- straight.el installs packages by default. Built-ins get `:straight (:type built-in)`; built-ins
  pulled in as dependencies go in `straight-built-in-pseudo-packages` in init.el.
- Prefer built-in Emacs features over new packages.
- Personal keys go on `z-map` in keybinds.el; personal helpers use the `db/` prefix.
- Evil is on; per-mode initial states live in the evil block in packages.el.
- Personal values (name, font, paths) go in the `globals--` block in init.el.
- No commented-out code; git has the history.

## Verify
Run `bin/check.sh` after every change. It boots the config in a throwaway terminal
Emacs and exits non-zero on init errors, warnings, or `Error` in `*Messages*`.
The first run after adding a package is slow while straight clones it.
