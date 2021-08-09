{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config;
in {
  config = {
    # Docker
    virtualisation.docker.enable = mkDefault true;
    virtualisation.docker.autoPrune.enable = mkDefault true;
    virtualisation.docker.autoPrune.dates = mkDefault "daily";
    environment.systemPackages = with pkgs; [ docker_compose ];
    # VirtualBox
    virtualisation.virtualbox.guest.enable = mkDefault false;
    virtualisation.virtualbox.host.enable = mkDefault (!cfg.virtualisation.virtualbox.guest.enable);
    virtualisation.virtualbox.host.enableHardening = true;
  };
}