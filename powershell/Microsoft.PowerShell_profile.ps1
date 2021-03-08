# Git aliases
Set-Alias g git
Set-Alias t tig

$Env:EDITOR = "nvim"

# coloured ls
Function LsColor {
    /run/current-system/sw/bin/ls --color @Args
}
Set-Alias l LsColor
Set-Alias ls LsColor

# FIXME https://github.com/kelleyma49/PSFzf/issues/73
# Enable-PsFzfAliases

# The minimal, blazing-fast, and infinitely customizable prompt for any shell!
# https://github.com/starship/starship
Invoke-Expression (&starship init powershell)
