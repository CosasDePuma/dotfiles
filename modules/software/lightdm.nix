{ config, lib, pkgs, ... }:
let
  name = "lightdm";
  cfg = config.software."${name}";
in {
  options = {
    software."${name}" = {
      enable = lib.mkEnableOption "custom ${name} (login manager)";

      autologin = lib.mkEnableOption "${name} autologin";

      background = lib.mkOption {
        type = lib.types.either lib.types.path (lib.types.strMatching "^#[0-9]\{6\}$");
        default = "#282828";
        example = ../../config/wallpapers/wallpaper.png;
        description = "Path to the ${name} background image or hex color.";
      };

      config = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        example = ../../config/${name};
        description = "The path of the ${name} configuration file or folder.";
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
    environment.etc = = lib.mkIf (cfg.config != null) {
      "${name}" = if (builtins.pathExists (builtins.toString cfg.config + "/.")) then {
        source = cfg.config;
        target = name;
      } else {
        text = builtins.readConfig cfg.config;
        target = "${name}/${name}-gtk-greeter.conf";
      };
    };
  };
}