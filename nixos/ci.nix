{ config, lib, pkgs, ... }:

{
  imports =
    [( builtins.fetchTarball "https://github.com/hercules-ci/hercules-ci-agent/archive/stable.tar.gz"
        + "/module.nix"
      )
    ];

  services.hercules-ci-agent = {
    enable = true;
    concurrentTasks = 4; # Number of jobs to run
    patchNix = true;
  };
}
