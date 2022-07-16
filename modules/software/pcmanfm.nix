{ config, lib, pkgs, ... }:
let
  cfg = config.software.pcmanfm;
in {
  options = {
    software.pcmanfm = {
      enable = lib.mkEnableOption "custom pcmanfm (file manager)";

      package = lib.mkPackageOption pkgs "pcmanfm" {};

      config = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        example = ../../config/pcmanfm/default/pcmanfm.conf;
        description = "The path of the pcmanfm configuration file.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      etc."pcmanfm/default/pcmanfm.conf" = lib.mkIf (cfg.config != null) {
        text = builtins.readFile cfg.config;
      };
    };
  };
}
