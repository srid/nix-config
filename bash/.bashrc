# Run twolfson/sexy-bash-prompt
. ~/.bash_prompt

alias j="nvim +AutoSaveToggle ~/private/journal.md"
alias n='nvim +AutoSaveToggle +Goyo $(find ~/Dropbox/Note -type f -name \*.md | fzf)'
alias e='nvim $(fzf)'
alias copy="xclip -i -selection clipboard"
alias ls='ls --color'  # TODO: osx?

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
