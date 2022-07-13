{ config, lib, pkgs, ... }:
let
  cfg = config.programs.alacritty;
in {
  options = {
    programs.alacritty = {
      enable = lib.mkEnableOption "custom alacritty";

      package = lib.mkPackageOption pkgs "alacritty" {};

      config = lib.mkOption {
        type = lib.types.path;
        default = ../../config/alacritty/alacritty.yml;
        description = "The path of the picom configuration file.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      etc."xdg/alacritty.yml".text = builtins.readFile cfg.config;
    };
  };
}
