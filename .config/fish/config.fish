set -x PATH $PATH $HOME/.local/bin
set -x PATH $PATH $HOME/.gem/ruby/2.5.0/bin/
set -x PATH $PATH $HOME/.yarn/bin/
set -x PATH $PATH $HOME/.cargo/bin/
set -x PATH $PATH $HOME/go/bin/

# set -g SSH_AUTH_SOCK $HOME/.gnupg/S.gpg-agent.ssh

# set -gx TERM 'xterm-256color'
set -gx EDITOR vim

status --is-interactive; and source (pyenv init -|psub)
status --is-interactive; and source (pyenv virtualenv-init -|psub)

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
