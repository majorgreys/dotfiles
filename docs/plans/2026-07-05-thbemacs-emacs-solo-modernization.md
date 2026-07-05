# thbemacs modernization research: emacs-solo comparison

Issue: `dotfiles-xn6`  
Source compared: `LionyxML/emacs-solo` at `16bed87` (`2026-07-02`, `feat: tab cycling won't get out of groups anymore`)  
Local target: `thbemacs/.config/thbemacs`

## Research strategy

1. Baseline thbemacs startup and dependency surface.
2. Close-read emacs-solo for patterns that explain its stated goals: fast startup, built-in-first design, modular local helpers, and low package count.
3. Compare across startup/runtime performance, lazy loading, built-in replacements, package manager behavior, and migration risk.
4. Convert findings into actionable, scoped changes that can be implemented independently.

## Baseline notes

- `thbemacs/init.el`: 2555 lines, 42 `use-package` forms.
- `thbemacs/early-init.el` already has solid startup basics: raised GC threshold, temporary `file-name-handler-alist` disable, `package-enable-at-startup nil`, early GUI chrome suppression, native-comp noise suppression.
- Warm daemon startup measurement after archive refresh: about `1.93s` Emacs init time, `2.64s` wall clock, `61` activated packages.
- Cold/stale archive startup measurement: about `6.72s` Emacs init time, `7.38s` wall clock, dominated by synchronous package archive refresh.
- `emacs-solo/init.el`: 4103 lines, 51 `use-package` forms, but mostly built-in packages and local `lisp/emacs-solo-*` modules; package count reported as `0` in its splash.

## Key emacs-solo patterns worth stealing

- Startup does not refresh package archives or install packages on the critical path.
- `early-init.el` sets `gc-cons-threshold` to `most-positive-fixnum`, restores later to `100MB`, restricts VC to Git, avoids initial light flash, and suppresses unused UI before first frame.
- Built-in-first package selection: `icomplete`, `minibuffer`, `completion-preview-mode`, `tab-bar`, `project`, `vc`, `diff-mode`, `ediff`, `eldoc`, `eglot`, `flymake`, `dired`, `wdired`, `org`, tree-sitter modes.
- Local modules replace small external packages where feasible: mode line, gutters, olivetti-like centering, icons, formatter, movement helpers, GitHub/container UIs.
- State/cache files are centralized under cache paths rather than scattered through the config directory.
- Expensive or private code is loaded after startup via idle timers or optional private files.

## Prioritized recommendations

### 1. Remove synchronous package refresh from startup

Change: replace the eager `(package-refresh-contents)` path in `init.el` with an explicit command such as `thb/package-refresh-if-stale`, or schedule it with an idle timer after startup. Keep missing-package install explicit for now.

Rationale: the measured cold/stale daemon run spent several seconds contacting MELPA/GNU/Nongnu before the daemon was available.

Expected impact: largest startup win; avoids network-dependent daemon failures.

Risk: missing packages will no longer be repaired before first use unless the explicit refresh/install command is run.

### 2. Defer org-roam/Vulpea autosync until after startup or first note command

Change: keep Org itself available, but defer `org-roam`, `vulpea`, `vulpea-ui`, `vulpea-journal`, and `consult-vulpea` behind commands and/or an idle timer. Start `vulpea-db-autosync-mode` after Emacs is responsive.

Rationale: thbemacs currently loads the note database stack at daemon startup because `org` is configured eagerly and the dependent packages follow it.

Expected impact: faster startup and less daemon boot fragility; fswatch/db work moves off the critical path.

Risk: first note command may pay a one-time load cost; journal/sidebar commands need autoload coverage.

### 3. Move Go support from external `go-mode` to Emacs 31 `go-ts-mode`

Change: replace the `go-mode` package with built-in `go-ts-mode`/`go-mod-ts-mode`, preserving the existing `eglot-ensure` and gopls workspace settings.

Rationale: emacs-solo uses tree-sitter Go modes; thbemacs already requires Emacs 31 and already invests in tree-sitter for Markdown.

Expected impact: removes one external package, modernizes syntax support, aligns Go with the rest of Emacs 31.

Risk: indentation and mode hooks differ slightly; ensure Go grammars are installed.

### 4. Audit the completion stack against Emacs 31 built-ins

Change: prototype a branch that replaces some of `vertico`, `orderless`, `marginalia`, `corfu`, and `cape` with Emacs 31 `icomplete-vertical-mode`, `completion-preview-mode`, built-in `flex`/custom orderless style, and tuned `minibuffer` variables. Keep `consult` initially because thbemacs uses `consult-org-heading`, `consult-ripgrep`, and `consult-vulpea`.

Rationale: emacs-solo demonstrates that modern built-in completion is viable and avoids several always-on packages.

Expected impact: medium package/dependency reduction; possible startup and maintenance win.

Risk: high UX/muscle-memory risk; Embark/Consult workflows may be worth keeping.

### 5. Replace `ultra-scroll` with built-in pixel scrolling if acceptable

Change: test `pixel-scroll-precision-mode` with `pixel-scroll-precision-use-momentum nil` before loading `ultra-scroll`.

Rationale: emacs-solo uses built-in pixel scrolling. thbemacs loads `ultra-scroll` eagerly.

Expected impact: removes one external eager package if built-in behavior is good enough.

Risk: trackpad feel may regress; this should be user-tested, not blindly changed.

### 6. Consider built-in `tab-bar`/`project` workspaces before keeping `tabspaces`

Change: prototype a small built-in tab-bar workspace layer inspired by emacs-solo and compare it with current `tabspaces` behavior.

Rationale: thbemacs uses `tabspaces` mostly to wrap built-in tab-bar/project primitives. emacs-solo implements workspace grouping directly with `tab-bar`.

Expected impact: dependency reduction and simpler moving parts.

Risk: medium/high; tab-local buffer filtering and Doom-like commands may take custom code to preserve.

### 7. Keep Magit for now, but import selected built-in VC improvements

Change: do not remove Magit immediately. Instead, add low-risk built-in VC/diff settings from emacs-solo: better `vc-git-diff-switches`, `vc-git-log-switches`, `diff-default-read-only`, `diff-update-on-the-fly`, `smerge` bindings, and optional `vc-dir` staging helpers.

Rationale: emacs-solo proves built-in VC can cover many tasks, but Magit replacement is a major workflow change.

Expected impact: improves built-in fallback tools and may allow later Magit-light experiments.

Risk: low if added as supplemental bindings/settings.

### 8. Centralize cache/state paths

Change: add a small `thb/cache-path` helper and move `recentf`, `savehist`, `save-place`, `tramp`, URL, native cache, and other state files under a cache directory.

Rationale: emacs-solo has an explicit cache path table. thbemacs currently keeps some generated state near the config, and `custom.el` is tracked.

Expected impact: cleaner repo/config directory and safer dotfile sync.

Risk: low; existing history files may need one-time migration.

### 9. Lazy-load local/private overrides

Change: keep `local.el` for machine overrides, but consider loading non-critical local/private code after startup with an idle timer, matching emacs-solo's `private.el` pattern.

Rationale: local customizations tend to accumulate slow machine-specific code.

Expected impact: small-to-medium depending on local file contents.

Risk: settings needed before package configuration must remain eager.

## Suggested implementation order

1. Package refresh off startup.
2. Go `go-ts-mode` migration.
3. Org-roam/Vulpea lazy-load/autosync deferral.
4. Built-in scroll trial.
5. Cache path cleanup.
6. Completion stack prototype.
7. Tabspaces prototype.
8. VC/Magit-light exploration.

The first three are the best near-term changes: clear rationale, limited scope, and measurable impact.
