#!/usr/bin/env bash 
set -xe 

NAME=pristine
lxc launch ubuntu:18.04 $NAME -c security.nesting=true
lxc exec $NAME -- useradd -s /bin/bash -G sudo -m  srid
lxc exec $NAME -- /bin/bash -c 'echo "srid ALL=(ALL) NOPASSWD:ALL" | tee -a /etc/sudoers'
lxc file push /home/srid/.ssh/id_rsa.pub ${NAME}/tmp/
lxc exec ${NAME} -- su - srid -c 'mkdir .ssh && cp /tmp/id_rsa.pub .ssh/authorized_keys'

echo "lxc exec ${NAME} -- su - srid -c \"tmux new-session -A -s main\""

