{ config, lib, pkgs, ... }:
let
  name = "redshift";
  cfg = config.software."${name}";
in {
  options = {
    software."${name}" = {
      enable = lib.mkEnableOption "custom ${name} (terminal emulator)";

      package = lib.mkPackageOption pkgs name {};

      latitude = lib.mkOption {
        type = lib.types.float;
        default = 51.29;
        description = "Current latitude.";
      };

      longitude = lib.mkOption {
        type = lib.types.float;
        default = 0.0;
        description = "Current longitude.";
      };
    };
  };

  config = {
    location.provider = "geoclue2";
    services.redshift = {
      enable = cfg.enable;
      package = cfg.package;
    };
  };
}
