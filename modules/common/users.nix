{ config, ... }:
let
  cfg = config;
in {
  config = {
    users.mutableUsers = true;
    services.xserver.displayManager.autoLogin.enable = (cfg.services.xserver.displayManager.autoLogin.user != null);
    # users.users.root.initialPassword = "CHANGEME"; # FIXME: Not working, still asking
  };
}
