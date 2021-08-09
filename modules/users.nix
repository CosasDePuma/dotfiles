{ config, lib, ... }:
with lib; {
  config = {
    users.mutableUsers = mkDefault true;
    users.users.root.initialPassword = mkDefault "CHANGEME";
    services.xserver.displayManager.autoLogin.enable = mkDefault (config.services.xserver.displayManager.autoLogin.user != null);
  };
}