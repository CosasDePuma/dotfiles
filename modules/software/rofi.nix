{ config, lib, pkgs, ... }:
let
  cfg = config.software.rofi;
in {
  options = {
    software.rofi = {
      enable = lib.mkEnableOption "custom rofi (launcher)";

      package = lib.mkPackageOption pkgs "rofi" {};

      config = lib.mkOption {
        type = lib.types.path;
        default = ../../config/rofi/rofi.rasi;
        description = "The path of the rofi configuration file.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      etc."rofi/rofi.rasi".text = builtins.readFile cfg.config;
    };
  };
}
