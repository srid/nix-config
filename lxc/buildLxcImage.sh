#! /usr/bin/env nix-shell
#! nix-shell -p nixos-generators
#! nix-shell -i bash
set -xe

CONFIGURATIONNIX=$1
METAIMG=`nixos-generate -f lxc-metadata \
  | xargs -r cat \
  | awk '{print $3}'`
IMG=`nixos-generate -c ${CONFIGURATIONNIX} -f lxc \
  | xargs -r cat \
  | awk '{print $3}'`

lxc image import --alias nixos ${METAIMG} ${IMG}
