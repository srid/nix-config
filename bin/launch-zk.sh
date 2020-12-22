code ~/Documents/zk &
myst -e tmux new-session -A -s zk "cd ~/Documents/zk && neuron rib -ws :8081" &
google-chrome-stable http://localhost:8081 &
