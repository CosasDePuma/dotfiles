{ config, lib, pkgs, ... }:
let
  name = "feh";
  cfg = config.software."${name}";
in {
  options = {
    software."${name}" = {
      enable = lib.mkEnableOption "custom ${name} (image viewer and background manager)";

      package = lib.mkPackageOption pkgs name {};

      wallpapers = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        example = ../../config/wallpapers;
        description = "The path of the default file or folder with the wallpaper(s).";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      etc = lib.mkIf (cfg.wallpapers != null) {
        "${name}" = {
          source = cfg.wallpapers;
          target = if (builtins.pathExists (builtins.toString cfg.wallpapers + "/."))
            then name else "${name}/wallpaper";
        };
      };
    };
  };
}