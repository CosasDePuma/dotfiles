{ config, lib, pkgs, ... }:
let
  cfg = config.software.feh;
in {
  options = {
    software.feh = {
      enable = lib.mkEnableOption "custom feh (image viewer and background manager)";

      package = lib.mkPackageOption pkgs "feh" {};

      wallpaper = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        example = ../../config/wallpapers/ghiblike.png;
        description = "The path of the default wallpaper.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      etc."wallpaper" = lib.mkIf (cfg.wallpaper != null) {
        source = cfg.wallpaper;
        target = "feh/wallpaper";
      };
    };
  };
}
