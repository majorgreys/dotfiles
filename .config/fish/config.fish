if test $HOME/.local/bin
    set -U fish_user_paths $HOME/.local/bin $fish_user_paths
end

if type -q yarn
    set -U fish_user_paths (yarn global bin) $fish_user_paths
end

if type -q cargo
    set -U fish_user_paths $HOME/.cargo/bin $fish_user_paths
end

if type -q go
    set -U GOROOT (go env GOROOT)
    set -U GOPATH (go env GOPATH)
    set -U fish_user_paths $GOROOT/bin $fish_user_paths
    set -U fish_user_paths $GOPATH/bin $fish_user_paths
end

if type -q exa
    abbr -a ls exa
end

# Base16 Shell
if status --is-interactive
    set BASE16_SHELL "$HOME/.config/base16-shell/"
    source "$BASE16_SHELL/profile_helper.fish"
end

# Set up fisherman
if not test -f ~/.config/fish/functions/fisher.fish
  echo "Installing fisherman for the first time"
  curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
  fisher
end
