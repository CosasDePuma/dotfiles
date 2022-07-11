{ config, lib, pkgs, ... }:
let
  cfg = config.desktop.picom;
  extraPkgs = with pkgs;[ dunst feh rofi ];
in {
  options = {
    desktop.picom = {
      enable = lib.mkEnableOption "custom picom";

      config = lib.mkOption {
        type = lib.types.path;
        default = ../../config/picom/picom.conf;
        description = "The path of the picom configuration file.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.picom = {
      enable = true;
      settings = builtins.readFile cfg.config;
    };
  };
}
