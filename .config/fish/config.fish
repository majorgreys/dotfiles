set PATH $PATH $HOME/.local/bin
set PATH $PATH $HOME/.gem/ruby/2.4.0/bin/

set -gx TERM 'xterm-256color'
set -gx CLICOLOR 1
set -gx EDITOR nvim

. $HOME/.config/fish/base16-material.dark.fish
. /opt/anaconda/etc/fish/conf.d/conda.fish
if not test -f ~/.config/fish/functions/fisher.fish
  echo "Installing fisherman for the first time"
  curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
  fisher
end
