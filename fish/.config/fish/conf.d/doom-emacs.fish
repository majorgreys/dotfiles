# doom-emacs — Emacs client for Doom Emacs daemon
# Daemon managed by LaunchAgent (com.thb.doom-emacs), socket name "doom".

function doom-emacs --description "Connect to Doom Emacs daemon"
    emacsclient --socket-name=doom --create-frame $argv
end

function doom-emacs-tui --description "Connect to Doom Emacs daemon in terminal mode"
    env TERM=xterm-256color COLORTERM=truecolor emacsclient --socket-name=doom -nw $argv
end
