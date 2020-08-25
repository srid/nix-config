#! /usr/bin/env nix-shell
#! nix-shell -p nixos-generators
#! nix-shell -i bash
set -xe

CONFIGURATIONNIX=$1
METAIMG=`nixos-generate -f lxc-metadata`
IMG=`nixos-generate -c ${CONFIGURATIONNIX} -f lxc`

lxc image delete nixos || echo true
lxc image import --alias nixos ${METAIMG} ${IMG}
