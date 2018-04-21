#!/bin/sh -xe
nix-shell -p psmisc --run "killall chrome" || echo "Nothing to kill"
# chrome &
