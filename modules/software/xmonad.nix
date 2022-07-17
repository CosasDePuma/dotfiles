{ config, lib, pkgs, ... }:
let
  name = "xmonad";
  cfg = config.software."${name}";
in {
  options = {
    software."${name}" = {
      enable = lib.mkEnableOption "custom ${name} (window manager)";

      config = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        example = ../../config/${name};
        description = "The path of the ${name} configuration file. This file is embedded in the binary.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.xserver = {
      enable = true;
      autorun = true;
      displayManager.defaultSession = "none+xmonad";
      windowManager.xmonad = {
        enable = true;
        config = cfg.config;
        enableContribAndExtras = true;
      };
    };
  };
}