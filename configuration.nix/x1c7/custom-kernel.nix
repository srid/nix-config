{ config, pkgs, ... }:

# You can also uses a different kernel from nixpkgs, eg:
#   boot.kernelPackages = pkgs.linuxPackages_testing;
# I use a custom kernel when it is not yet available in nixpkgs for whatever
# reason, or when I'm lazy to uptream it.
{
  boot.kernelPackages = let
      customLinuxF = { fetchurl, buildLinux, ... } @ args:
        buildLinux (args // rec {
          version = "5.9.9";
          extraMeta.branch = "5.9";
          modDirVersion = "${version}";

          src = fetchurl {
            url = "https://cdn.kernel.org/pub/linux/kernel/v5.x/linux-${version}.tar.xz";
            sha256 = "1b8zysy0br131ydhc7ycxhh8d88r44xrmkf2q2lffy0jmy3d60m3";
          };
          kernelPatches = [];
        } // (args.argsOverride or {}));
      customLinux = pkgs.callPackage customLinuxF{};
    in
      pkgs.recurseIntoAttrs (pkgs.linuxPackagesFor customLinux);
}
