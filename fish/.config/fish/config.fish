set fish_greeting
set -e fish_user_paths

if test $HOME/.local/bin
   set -x fish_user_paths $HOME/.local/bin/ $fish_user_paths
end

if set -q $ASDF_DIR
   set -x fish_user_paths $ASDF_DIR/bin/ $fish_user_paths
   source $ASDF_DIR/asdf.fish
end

if type -q yarn
    set -x fish_user_paths (yarn global bin) $fish_user_paths
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

if type -q exa
    abbr -a ls exa
end

# Set up fisherman
if not test -f ~/.config/fish/functions/fisher.fish
  echo "Installing fisherman for the first time"
  curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
  fisher
end
