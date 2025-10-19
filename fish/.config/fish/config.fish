set fish_greeting

# PATH additions
fish_add_path -g ~/.local/bin
fish_add_path -g ~/.cargo/bin
fish_add_path -g /opt/homebrew/bin

# Set up fisherman
if not test -f ~/.config/fish/functions/fisher.fish
    echo "Installing fisherman for the first time"
    curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
    fisher
end

direnv hook fish | source
zoxide init fish | source
starship init fish | source
