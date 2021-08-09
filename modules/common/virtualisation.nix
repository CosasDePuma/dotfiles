{ config, pkgs, ... }:{
  config = {
    virtualisation = {
      # Docker
      docker.enable = true;
      docker.autoPrune.enable = true;
      docker.autoPrune.dates = "daily";
      environment.systemPackages = with pkgs; [ docker_compose ];
      # VirtualBox
      virtualbox.host.enable = false;
      virtualbox.host.enableHardening = true;
    };
  };
}