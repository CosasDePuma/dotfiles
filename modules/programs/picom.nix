{ config, lib, pkgs, ... }:
let
  cfg = config.programs.picom;
in {
  options = {
    programs.picom = {
      enable = lib.mkEnableOption "custom picom";

      package = lib.mkPackageOption pkgs "picom" {};

      config = lib.mkOption {
        type = lib.types.path;
        default = ../../config/picom/picom.conf;
        description = "The path of the picom configuration file.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      etc."xdg/picom.conf".text = builtins.readFile cfg.config;
    };
  };
}
