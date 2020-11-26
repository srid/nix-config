{ pkgs, config, ...}:

# Refs:
#  - https://nixos.wiki/wiki/PulseAudio
#  - https://discourse.nixos.org/t/install-sound-open-firmware/6535/13
{
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  nixpkgs.config.pulseaudio = true; # Explicit PulseAudio support in applications
  environment.systemPackages = with pkgs; [
    pulsemixer
  ];
}
