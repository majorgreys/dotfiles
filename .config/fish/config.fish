set PATH $PATH $HOME/.local/bin
set PATH $PATH $HOME/.gem/ruby/2.4.0/bin/
# set PATH $PATH $HOME/anaconda3/bin/
set PATH $PATH $HOME/.yarn/bin/

set -gx TERM 'xterm-256color'
set -gx CLICOLOR 1
set -gx EDITOR vim

. $HOME/anaconda3/etc/fish/conf.d/conda.fish

# Base16 Shell
if status --is-interactive
    eval sh $HOME/.config/base16-shell/scripts/base16-nord.sh
end

# Set up fisherman
if not test -f ~/.config/fish/functions/fisher.fish
  echo "Installing fisherman for the first time"
  curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
  fisher
end
