export TERM=xterm-256color
export EDITOR=nvim
export PATH=~/.nix-profile/bin:~/.local/bin:$PATH

# eval `keychain --eval id_rsa`

alias copy="xclip -i -selection clipboard"
alias ls='ls -G'

source .nix-profile/etc/profile.d/nix.sh 

# Trigger ~/.bashrc commands
. ~/.bashrc

alias j="nvim '+normal G$' ~/private/journal.md"
