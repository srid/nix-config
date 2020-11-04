# Import this to leverage P71 as a builder
{ config, pkgs, ... }:

let 
  bornagainIp = "192.168.2.127";
in {
  nix.buildMachines = [ {
    hostName = bornagainIp;
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
}
