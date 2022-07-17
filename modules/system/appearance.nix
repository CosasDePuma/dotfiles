{ config, lib, pkgs, ... }:
let
  name = "appearance";
  cfg = config.system."${name}";
in {
  options = {
    system."${name}" = {
      themes = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [];
        example = [ pkgs.theme-vertex ];
        description = "List of themes to be installed.";
      };

      icons = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [];
        example = [ pkgs.papirus-icon-theme ];
        description = "List of icon themes to be installed.";
      };

      fonts = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [];
        example = [ pkgs.nerdfonts ];
        description = "List of fonts to be installed.";
      };

      config = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        example = ../../config/gtk-3.0;
        description = "The path of the GTK 3 configuration file or folder.";
      };
    };
  };

  config = {
    fonts.fonts = cfg.fonts;
    xdg.icons.enable = (cfg.icons != []);
    environment = {
      systemPackages = cfg.themes ++ cfg.icons;
      etc = lib.mkIf (cfg.config != null) {
        "${name}" = if (builtins.pathExists (builtins.toString cfg.config + "/.")) then {
          source = cfg.config;
          target = "xdg/gtk-3.0";
        } else {
          text = builtins.readConfig cfg.config;
          target = "xdg/gtk-3.0/settings.ini";
        };
      };
    };
  };
}