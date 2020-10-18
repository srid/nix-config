{ config, pkgs, ...}:

{
  virtualisation.docker.enable = true;
  environment.systemPackages = with pkgs; [
    # Useful for vscode dev.
    docker-compose
  ];
}
