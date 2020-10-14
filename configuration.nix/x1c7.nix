{ config, pkgs, ... }:

let 
  hostName = "bebe";
  nixpkgs-master = import ../nixos/nixpkgs-master.nix;
in {
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix

      <nixos-hardware/lenovo/thinkpad>
      <nixos-hardware/lenovo/thinkpad/x1>
      <nixos-hardware/lenovo/thinkpad/x1/7th-gen>
      <home-manager/nixos>

      ../nixos/quebec.nix
      ../nixos/nix.nix
      ../nixos/nix-distributed.nix
      ../nixos/gnome.nix
      ../nixos/tmux.nix
      ../nixos/fonts.nix
      ../nixos/syncthing-firewall.nix

      ../private-config/caches.nix
    ];

  home-manager.users.srid = (import ../nix/home.nix {
    inherit pkgs config hostName;
  } );
  home-manager.users.root = (import ../nix/root-home.nix {
    inherit pkgs config;
  } );

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # boot.kernelPackages = pkgs.linuxPackages_testing;
  boot.kernelPackages = let
      customLinuxF = { fetchurl, buildLinux, ... } @ args:

        buildLinux (args // rec {
          version = "5.9";
          extraMeta.branch = "5.9";
          modDirVersion = "${version}.0";

          src = fetchurl {
            url = "https://git.kernel.org/torvalds/t/linux-${version}.tar.gz";
            sha256 = "0z7mmbwghhd93rs7b51lw71zbny109bmvc0f6r475yjl2sc5g2r3";
          };
          kernelPatches = [];
        } // (args.argsOverride or {}));
      customLinux = pkgs.callPackage customLinuxF{};
    in 
      pkgs.recurseIntoAttrs (pkgs.linuxPackagesFor customLinux);
  
  # Allow non-free firmware, such as for intel wifi
  hardware = {
    enableRedistributableFirmware = true;
  };
  # Sound
  sound.enable = true;
  # Fingerprint reader
  services.fwupd.enable = true;
  services.fprintd.enable = true;
  security.pam.services.login.fprintAuth = true;
  security.pam.services.xscreensaver.fprintAuth = true;

  networking = {
    hostName = hostName;
    networkmanager.enable = true;
    wireless.networks = ./private-config/wifi.nix;
    firewall.enable = false;

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    interfaces.wlp0s20f3.useDHCP = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # google-chrome UI lags on x1c7
    # google-chrome
    chromium
    peek
    mpv
    nixpkgs-master.vscode
    nodejs-12_x
    /* (let 
      extensions = (with nixpkgs-master.vscode-extensions; [
        ms-vscode-remote.remote-ssh
      ]);
    in nixpkgs-master.vscode-with-extensions.override {
      vscodeExtensions = extensions;
    }
      )*/
  ];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  virtualisation.docker.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    # Enable touchpad support.
    libinput.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.srid = {
     isNormalUser = true;
     extraGroups = [ "wheel" "docker" ];
     shell = pkgs.fish;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
