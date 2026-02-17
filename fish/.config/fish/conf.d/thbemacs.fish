# thbemacs — vanilla Emacs (--init-directory) launcher functions
# These coexist with Doom Emacs — default `emacs` command remains Doom.

function ev --description "Launch thbemacs GUI"
    /Applications/Emacs.app/Contents/MacOS/Emacs --init-directory ~/.config/thbemacs/ $argv &
    disown
end

function evd --description "Start thbemacs daemon"
    /Applications/Emacs.app/Contents/MacOS/Emacs --init-directory ~/.config/thbemacs/ --daemon=vanilla
end

function evc --description "Connect to thbemacs daemon"
    emacsclient --socket-name=vanilla --create-frame $argv
end
