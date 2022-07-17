{ config, lib, pkgs, ... }:
let
  name = "qemu";
  cfg = config.software."${name}";
in {
  options = {
    software."${name}" = {
      enable = lib.mkEnableOption "custom ${name} (vm manager)";

      package = lib.mkPackageOption pkgs name {};
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];
  };
}