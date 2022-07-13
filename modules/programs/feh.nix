{ config, lib, pkgs, ... }:
let
  cfg = config.programs.feh;
in {
  options = {
    programs.feh = {
      enable = lib.mkEnableOption "custom feh (image viewer and background manager)";

      package = lib.mkPackageOption pkgs "feh" {};

      wallpaper = lib.mkOption {
        type = lib.types.path;
        default = ../../config/wallpapers/sagiri.png;
        description = "The path of the default wallpaper.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      etc."wallpaper".source = cfg.wallpaper;
      etc."wallpaper".target = "feh/wallpaper";
    };
  };
}
