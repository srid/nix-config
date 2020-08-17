{ config, lib, pkgs, ... }:

let
  go-cast = pkgs.callPackage (builtins.fetchTarball {
    url = "https://github.com/srid/go-cast/archive/df48bea685.tar.gz";
    sha256 = "0xka0q3f7axrglckaxbmh0cwpad1r7kc1im4alfhw9vp90j81xyh";
  }) {};
  shellAliases = {
    cast-mini = "${go-cast}/bin/cast --name SridMini ";
    cast-screen = "${go-cast}/bin/cast --name SridScreen ";
    cast-screen-castnow = "${pkgs.nodePackages.castnow} --address 192.168.2.64 --myip 192.168.2.204";
    cast = "${go-cast}/bin/cast";
  };

in
{
  programs.fish = {
    inherit shellAliases;
  };

  programs.bash = {
    inherit shellAliases;
  };
}
