{ config, lib, pkgs, ... }:
let
  name = "coolretroterm";
  cfg = config.software.${name};
in {
  options = {
    software."${name}" = {
      enable = lib.mkEnableOption "custom ${name} (terminal emulator)";

      package = lib.mkPackageOption pkgs "cool-retro-term" {};
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];
  };
}