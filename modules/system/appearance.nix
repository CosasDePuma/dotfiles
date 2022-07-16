{ config, lib, pkgs, ... }:
let
  cfg = config.system.appearance;
in {
  options = {
    system.appearance = {
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
        example = ../../config/gtk-3.0/settings.ini;
        description = "The path of the GTK 3 configuration file.";
      };
    };
  };

  config = {
    fonts.fonts = cfg.fonts;
    environment = {
      systemPackages = cfg.themes ++ cfg.icons;
      etc."xdg/gtk-3.0/settings.ini" = lib.mkIf (cfg.config != null) {
        text = builtins.readFile cfg.config;
      };
    };
  };
}
