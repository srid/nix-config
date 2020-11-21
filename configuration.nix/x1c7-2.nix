{ config, pkgs, ... }:

let 
  hostName = "bebe";
  nixpkgs-master = import ../nixos/nixpkgs-master.nix;
in {
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix

      # Get community hardware configuration
      <nixos-hardware/lenovo/thinkpad>
      <nixos-hardware/lenovo/thinkpad/x1>
      <nixos-hardware/lenovo/thinkpad/x1/7th-gen>
      # home-manager
      <home-manager/nixos>

      # Essential imports
      ../nixos/nix.nix
      ../nixos/quebec.nix
      ../nixos/shell.nix
      ../nixos/tmux.nix

      # Other imports
      ../nixos/fonts.nix
      ../nixos/gnome.nix
      ../nixos/i3.nix
      ../nixos/redshift.nix
      ../nixos/syncthing.nix
      ../nixos/syncthing-tray.nix
      
      # Remote building (because X1C7 is not the fastest)
      ../nixos/nix-distributed.nix

      ../private-config/caches.nix
    ];

  home-manager.users.srid = (import ../nix/home.nix {
    inherit pkgs config hostName;
  } );

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # boot.kernelPackages = pkgs.linuxPackages_testing;
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

  #fileSystems."/".device = "/dev/mapper/crypted";

  networking.hostName = hostName;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  # Enable sound.
  sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Set up Wireguard
  networking = {
    firewall = {
      enable = true;
      allowedUDPPorts = [51820];
    };
    wireguard.interfaces = {
      wg0 = {
        ips = [ "10.100.0.3/24" ];
        listenPort = 51820;
        privateKeyFile = "/home/srid/nix-config/private-config/wireguard/x1c7/private";
        peers = [
          ../nixos/wireguard/peers/facade.nix
          ../nixos/wireguard/peers/p71.nix
        ];
      };
    };
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  users.users.srid = {
     isNormalUser = true;
     extraGroups = [ "wheel" "docker" ];
     shell = pkgs.bash;
     packages = with pkgs; [
       slack
       chromium
       vscode
       qmmp
       mpv
       peek
     ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

