export TERM=xterm-256color

# eval `keychain --eval id_rsa`

alias copy="xclip -i -selection clipboard"

export PATH=~/.local/bin:$PATH
# Trigger ~/.bashrc commands
. ~/.bashrc

alias j="nvim '+normal G$' ~/private/journal.md"
