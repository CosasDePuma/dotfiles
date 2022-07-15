{ config, lib, pkgs, ... }:
let
  cfg = config.software.coolretroterm;
in {
  options = {
    software.coolretroterm = {
      enable = lib.mkEnableOption "custom coolretroterm (terminal emulator)";

      package = lib.mkPackageOption pkgs "cool-retro-term" {};
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
    };
  };
}
