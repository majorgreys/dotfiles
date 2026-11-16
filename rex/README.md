# Rex Open in Emacs

Mac-side Rex configuration for opening original files and directories in GUI thbemacs. The palette action replaces the mddog preview action and does not bind Cmd+Shift+M.

## Behavior

- **Open in Emacs…** queues the Emacs file/directory picker.
- An explicit `path` argument opens a file or Dired directly.
- Local Pi panes start in their verified session artifact directory, or the verified conversation cwd when artifacts do not exist yet. Ordinary terminal panes start in the foreground process cwd.
- Remote panes start in their foreground cwd through TRAMP. Remote conversation-output suggestions are not implemented in this slice.
- Unknown Rex hosts fail instead of treating a remote path as local.
- The result says **queued**, not opened: selection, cancellation and SSH prompts happen in Emacs after the Rex action returns.
- Emacs owns buffer selection, frame reuse, workspace placement and final focus through `thb-open`.

This does not change Pi /open, intercept file-link clicks, download files, or install a global URL handler.

## Prerequisites and configuration

Requires the existing Mac thbemacs daemon with thb-open loaded, AeroSpace, the configured emacsclient, and an existing SSH alias for each supported remote host. Local Pi context discovery also uses the existing rex-agent-recovery controller at \~/dev/rex-agent-recovery.

The host IDs in init.lua are Rex **server host\_id** values reported by rex whoami, not host-list entry IDs. They are intentionally explicit: configure additional machines rather than guess SSH destinations. Unknown or changed identities fail closed.

Other pre-existing palette integrations are preserved. Their controller directories and the existing upgrade helper under \~/.config/rex/scripts remain prerequisites; this package does not install those projects.

## Verification

From the dotfiles root:

```sh
lua rex/test/open-emacs-test.lua rex/.config/rex/open-emacs.lua
node --test rex/test/pi-artifacts.test.mjs
rex config check rex/.config/rex/init.lua --json
```

The Lua and Node tests do not open a GUI, connect to a remote host, or modify live configuration. Config checking only loads definitions. Test actual focus, palette invocation and TRAMP access separately.

## Deployment

The current \~/.config/rex directory predates dotfiles ownership. Do not use blanket stow adoption or overwrite it.

After source review and explicit live-configuration approval, preserve the existing init.lua and pi-artifacts.mjs in the owning session's artifacts, then replace only those files with symlinks to this package and add the new open-emacs.lua symlink. Preserve scripts, mddog files and all unrelated configuration. Check for concurrent changes before replacing anything, validate the installed configuration, and explicitly reload the Mac Rex configuration. Do not restart Rex or touch the remote server configuration.

This migration is intentionally manual; setup.sh does not adopt the existing directory automatically.
