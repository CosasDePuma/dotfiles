{ config, lib, pkgs, ... }:
let
  cfg = config.software.lightdm;
in {
  options = {
    software.lightdm = {
      enable = lib.mkEnableOption "custom LightDM (login manager)";

      autologin = lib.mkEnableOption "LightDM autologin";

      background = lib.mkOption {
        type = lib.types.either lib.types.path (lib.types.strMatching "^#[0-9]\{6\}$");
        default = "#282828";
        description = "LightDM background image or color.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.xserver.displayManager.lightdm = {
      enable = true;
      background = cfg.background;
      autoLogin = lib.mkIf cfg.autologin {
        timeout = 10;
      };
    };
  };
}
