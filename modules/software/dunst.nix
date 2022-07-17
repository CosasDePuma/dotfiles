{ config, lib, pkgs, ... }:
let
  cfg = config.software.dunst;
in {
  options = {
    software.dunst = {
      enable = lib.mkEnableOption "custom dunst (launcher)";

      package = lib.mkPackageOption pkgs "dunst" {};

      config = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        example = ../../config/dunst/dunstrc;
        description = "The path of the dunst configuration file.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      etc."dunst" = lib.mkIf (cfg.config != null) {
        source = ../../config/dunst; # builtins.readFile cfg.config;
      };
    };
  };
}
