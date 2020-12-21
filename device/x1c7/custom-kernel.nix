{ config, pkgs, ... }:

# You can also uses a different kernel from nixpkgs, eg:
#   boot.kernelPackages = pkgs.linuxPackages_testing;
# I use a custom kernel when it is not yet available in nixpkgs for whatever
# reason, or when I'm lazy to uptream it.
{
  boot.kernelPackages = let
      customLinuxF = { fetchurl, buildLinux, ... } @ args:
        buildLinux (args // rec {
          version = "5.9.16";
          extraMeta.branch = "5.9";
          modDirVersion = "${version}";

          src = fetchurl {
            url = "https://cdn.kernel.org/pub/linux/kernel/v5.x/linux-${version}.tar.xz";
            sha256 = "sha256:11mbnjvb5d5gwbrwlkqvzpg1ij4m19l5wr3wca9iiyg5i2papmxh";
          };
          kernelPatches = [];
        } // (args.argsOverride or {}));
      customLinux = pkgs.callPackage customLinuxF{};
    in
      pkgs.recurseIntoAttrs (pkgs.linuxPackagesFor customLinux);
}
