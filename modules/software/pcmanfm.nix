{ config, lib, pkgs, ... }:
let
  cfg = config.software.pcmanfm;
in {
  options = {
    software.pcmanfm = {
      enable = lib.mkEnableOption "custom pcmanfm (file manager)";

      package = lib.mkPackageOption pkgs "pcmanfm" {};
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
    };
  };
}
