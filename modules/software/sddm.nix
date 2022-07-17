{ config, lib, pkgs, ... }:
let
  name = "sddm";
  cfg = config.software."${name}";
in {
  options = {
    software."${name}" = {
      enable = lib.mkEnableOption "custom ${name} (login manager)";

      autologin = lib.mkEnableOption "${name} autologin";

      theme = {
        name = lib.mkOption {
          type = lib.types.str;
          default = "";
          description = "${name} theme to be used.";
        };

        package = lib.mkOption {
          type = lib.types.nullOr lib.types.package;
          default = null;
          description = "${name} theme to be installed.";
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