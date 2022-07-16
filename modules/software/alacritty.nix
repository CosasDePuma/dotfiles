{ config, lib, pkgs, ... }:
let
  cfg = config.software.alacritty;
in {
  options = {
    software.alacritty = {
      enable = lib.mkEnableOption "custom alacritty (terminal emulator)";

      package = lib.mkPackageOption pkgs "alacritty" {};

      config = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        example = ../../config/alacritty/alacritty.yml;
        description = "The path of the alacritty configuration file.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      etc."xdg/alacritty.yml" = lib.mkIf (cfg.config != null) {
        text = builtins.readFile cfg.config;
      };
    };
  };
}
