{ config, pkgs, ... }:
let
  user = "bug";
in {
  imports = [
    ../../modules/i3.nix
    ../../modules/hacking.nix
  ];

  # System
  networking.hostName = "bounty";

  # User
  users.users."${user}" = {
    uid = 1000;
    createHome = true;
    isNormalUser = true;
    description = "Researcher";
    initialPassword = "CHANGEME";
    extraGroups = [ "network" "wheel" "wireshark" ];
  };
  services.xserver.displayManager.autoLogin.user = "${user}";
}