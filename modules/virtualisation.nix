{ config, lib, pkgs, ... }:
with lib; {
  config = {
    # Docker
    virtualisation.docker.enable = mkDefault true;
    virtualisation.docker.autoPrune.enable = mkDefault true;
    virtualisation.docker.autoPrune.dates = mkDefault "daily";
    environment.systemPackages = with pkgs; [ docker_compose ];
    # VirtualBox
    virtualisation.virtualbox.guest.enable = mkDefault false;
    virtualisation.virtualbox.host.enable = mkDefault (!config.virtualisation.virtualbox.guest.enable);
    virtualisation.virtualbox.host.enableHardening = true;
  };
}