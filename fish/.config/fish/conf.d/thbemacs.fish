# thbemacs — vanilla Emacs (--init-directory) launcher functions
# These coexist with Doom Emacs — default `emacs` command remains Doom.
# `ev` starts a daemon if not running, then connects via emacsclient.

function ev --description "Start thbemacs daemon and connect"
    if not emacsclient --socket-name=vanilla --eval '(daemonp)' >/dev/null 2>&1
        /Applications/Emacs.app/Contents/MacOS/Emacs --init-directory ~/.config/thbemacs/ --daemon=vanilla
    end
    emacsclient --socket-name=vanilla --create-frame $argv
end

function evc --description "Connect to thbemacs daemon"
    emacsclient --socket-name=vanilla --create-frame $argv
end

function eve --description "Eval elisp in thbemacs daemon"
    emacsclient --socket-name=vanilla --eval $argv
end
