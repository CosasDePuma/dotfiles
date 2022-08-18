{ config, lib, pkgs, ... }:
let
  name = "mate";
  cfg = config.software."${name}";
in {
  options = {
    software."${name}" = {
      enable = lib.mkEnableOption "custom ${name} (desktop manager)";
    };
  };

  config = lib.mkIf cfg.enable {
    services.xserver = {
      enable = true;
      autorun = true;
      desktopManager.mate.enable = true;
      displayManager.defaultSession = name;
    };
  };
}