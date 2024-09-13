set fish_greeting

if test $HOME/.local/bin
    fish_add_path $HOME/.local/bin/
end

if type -q cargo
    set -x fish_user_paths $HOME/.cargo/bin $fish_user_paths
end

if type -q go
    set -x GOROOT (go env GOROOT)
    set -x GOPATH (go env GOPATH)
    set -x fish_user_paths $GOROOT/bin $fish_user_paths
    set -x fish_user_paths $GOPATH/bin $fish_user_paths
end

if type -q eza
    abbr -a ls eza
end

if type -q helix
    abbr -a hx helix
end

# Set up fisherman
if not test -f ~/.config/fish/functions/fisher.fish
    echo "Installing fisherman for the first time"
    curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
    fisher
end

# Needed for tramp
if test $TERM = dumb
    function fish_prompt
        echo "\$ "
    end
    exec sh
end

direnv hook fish | source
