{ config, pkgs, ... }:{
  config = {
    environment.systemPackages = with pkgs; [ docker_compose ];
    virtualisation = {
      # Docker
      docker.enable = true;
      docker.autoPrune.enable = true;
      docker.autoPrune.dates = "daily";
      # VirtualBox
      virtualbox.host.enable = false;
      virtualbox.host.enableHardening = true;
    };
  };
}
