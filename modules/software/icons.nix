{ config, lib, pkgs, ... }:
let
  cfg = config.software.icons;
in {
  options = {
    software.icons = {
      enable = lib.mkEnableOption "custom icons theme";

      packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = with pkgs;[ papirus-icon-theme ];
        description = "Custom icon packages to be installed.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = cfg.packages;
    };
  };
}
