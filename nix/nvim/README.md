# Customized neovim

Based on [Ali Abrar](https://github.com/ali-abrar)'s Vim configuration.

## Build and test

```
nix-build
result/bin/nvim ...
```

## Install declaratively

Inside the `environment.systemPackages` list of your Nix configuration, add:

```
(callPackage (import /path/to/nvim/default.nix) {})
```
