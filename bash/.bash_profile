export TERM=xterm-256color

eval `keychain --eval id_rsa`

export PATH=~/.local/bin:$PATH
# Trigger ~/.bashrc commands
. ~/.bashrc
