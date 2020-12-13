# Import this to leverage P71 as a builder
# https://nixos.wiki/wiki/Distributed_build
{ config, pkgs, ... }:

{
  nix.buildMachines = [ {
    hostName = "thebeast";
    system = "x86_64-linux";
    maxJobs = 4;
    speedFactor = 2;
    supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
  }];
  nix.distributedBuilds = true;
  # Builder has fast internet connection
  nix.extraOptions = ''
    builders-use-substitutes = true
  '';

  # SSH'ing to bulid machine is done as root; so we must configure passwordless
  # ssh on root, which requires managing root's home directory (via
  # home-manager). Don't forget to manually set .ssh in root.
  home-manager.users.root = (import ../nix/root-home.nix {
    inherit pkgs config;
  } );
}
