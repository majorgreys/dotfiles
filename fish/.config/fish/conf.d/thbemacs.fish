# thbemacs — Emacs client (--init-directory ~/.config/thbemacs)
# Daemon managed by LaunchAgent (com.thbemacs.daemon).
# Coexists with Doom Emacs — default `emacs` command remains Doom.

function thbemacs --description "Connect to thbemacs daemon"
    emacsclient --socket-name=thbemacs --create-frame $argv
end

function thbemacs-tui --description "Connect to thbemacs daemon in terminal mode"
    env TERM=xterm-256color COLORTERM=truecolor emacsclient --socket-name=thbemacs -nw $argv
end
