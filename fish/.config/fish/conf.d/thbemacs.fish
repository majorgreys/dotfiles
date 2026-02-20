# thbemacs — Emacs client (--init-directory ~/.config/thbemacs)
# Daemon managed by LaunchAgent (com.thbemacs.daemon).
# Coexists with Doom Emacs — default `emacs` command remains Doom.

function thbemacs --description "Connect to thbemacs daemon"
    emacsclient --socket-name=thbemacs --create-frame $argv
end
