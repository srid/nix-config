export TERM=xterm-256color
export EDITOR=nvim
export PATH=~/.nix-profile/bin:~/.local/bin:~/mynixos/bin:$PATH

# eval `keychain --eval id_rsa`

if [[ $OSTYPE == darwin* ]]; then
  source .nix-profile/etc/profile.d/nix.sh 
fi

# Trigger ~/.bashrc commands
. ~/.bashrc

