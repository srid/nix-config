code --folder-uri "vscode-remote://ssh-remote+thebeast/home/srid/code/neuron" &
code --folder-uri "vscode-remote://ssh-remote+thebeast/home/srid/code/neuron/impulse" &
myst -e ssh -t thebeast "tmux att" &

