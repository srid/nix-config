{ config, lib, pkgs, ... }:

{
  imports =
    [( builtins.fetchTarball "https://github.com/hercules-ci/hercules-ci-agent/archive/stable.tar.gz"
        + "/module.nix"
      )
    ];

  # To avoid recompiling the agent:
  #   cachix use hercules-ci
  services.hercules-ci-agent.enable = true;
  services.hercules-ci-agent.concurrentTasks = 4; # Number of jobs to run
}
