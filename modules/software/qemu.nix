{ config, lib, pkgs, ... }:
let
  cfg = config.software.qemu;
in {
  options = {
    software.qemu = {
      enable = lib.mkEnableOption "custom qemu (vm manager)";

      package = lib.mkPackageOption pkgs "qemu" {};
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];
  };
}
