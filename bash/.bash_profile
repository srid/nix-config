export TERM=xterm-256color
export EDITOR=nvim
export PATH=~/.nix-profile/bin:~/.local/bin:$PATH

# eval `keychain --eval id_rsa`

source .nix-profile/etc/profile.d/nix.sh 

# Trigger ~/.bashrc commands
. ~/.bashrc

