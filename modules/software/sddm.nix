{ config, lib, pkgs, ... }:
let
  cfg = config.software.sddm;
in {
  options = {
    software.sddm = {
      enable = lib.mkEnableOption "custom SDDM (login manager)";

      autologin = lib.mkEnableOption "SDDM autologin";

      theme = {
        name = lib.mkOption {
          type = lib.types.str;
          default = "";
          description = "SDDM theme to be used.";
        };

        package = lib.mkOption {
          type = lib.types.nullOr lib.types.package;
          default = null;
          description = "SDDM theme to be installed.";
        };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.xserver.displayManager.sddm = {
      enable = true;
      autoNumlock = true;
      theme = cfg.theme.name;

      autoLogin = lib.mkIf cfg.autologin {
        relogin = true;
        minimumUid = 1000;
      };
    };

    environment.systemPackages = lib.optional (cfg.theme.package != null) cfg.theme.package;
  };
}
