# Customized neovim

Based on [Ali Abrar](https://github.com/ali-abrar)'s Vim configuration.

## Bulid and test

```
nix-build
result/bin/nvim ...
```

## Install declariatively

Inside the `environment.systemPackages` list of your Nix configuration, add:

```
(callPackage (import /path/to/nvim/default.nix) {})
```
