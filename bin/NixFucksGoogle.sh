#!/usr/bin/env bash
# https://discourse.nixos.org/t/chrome-os-83-breaks-nix-sandboxing/6764/3

set -ex
sudo umount /proc/{cpuinfo,diskstats,meminfo,stat,uptime}
