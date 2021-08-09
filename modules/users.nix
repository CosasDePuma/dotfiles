{ config, lib, ... }:
with lib;
let
  cfg = config;
in {
  config = {
    users.mutableUsers = mkDefault true;
    users.users.root.initialPassword = mkDefault "CHANGEME";
    services.xserver.displayManager.autoLogin.enable = mkDefault (cfg.services.xserver.displayManager.autoLogin.user != null);
  };
}