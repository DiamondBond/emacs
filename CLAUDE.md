# Emacs config

Personal Emacs 31 config.

## Layout

- `early-init.el`: startup tuning (GC held off until gcmh takes over; no package.el)
- `init.el`: straight.el bootstrap, the `globals--*` block, then `(require ...)` of each `lisp/` module
- `lisp/base.el`: built-in settings
- `lisp/keybinds.el`: the `C-1` prefix (`z-map`) and global keys
- `lisp/packages.el`: packages, in sections: core, evil, editing, completion, files, git, terminal, UI, org, apps
- `lisp/languages.el`: eglot, tree-sitter, language modes
- `lisp/functions.el`: personal commands
- `custom.el`: written by Custom

## Conventions

- Tabs for indentation, matching the existing files.
- One `use-package` block per package, under a short `;; Description` comment.
- straight.el installs packages by default, so blocks don't say `:straight t`. Built-ins get `:straight (:type built-in)`; built-ins pulled in as dependencies go in `straight-built-in-pseudo-packages` in init.el.
- Options go in `:custom`. Keep `setq` for values that are conditional, belong with code in `:config`, or would make `:custom` load the package at startup (autoloaded options such as `gnus-select-method`).
- Use an option's current name: `:custom` can't set a variable that only becomes an alias when its package loads (e.g. `org-startup-with-inline-images` is now `org-startup-with-link-previews`).
- Don't restate package defaults.
- Prefer built-in Emacs features over new packages.
- Personal keys go on `z-map` in keybinds.el; personal helpers use the `db/` prefix.
- Evil is on; per-mode initial states live in the evil block in packages.el.
- Personal values (name, font, paths) go in the `globals--` block in init.el.
- No commented-out code; git has the history.

## Verify

Run `bin/check.sh` after every change. It boots the config in a throwaway terminal Emacs and exits non-zero on init errors, warnings, or `Error` in `*Messages*`. The first run after adding a package is slow while straight clones it.
