{ config, lib, pkgs, ... }:
let
  cfg = config.software.bat;
in {
  options = {
    software.bat = {
      enable = lib.mkEnableOption "custom bat (better cat)";

      package = lib.mkPackageOption pkgs "bat" {};
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      shellAliases = {
        "cat"  = "bat --paging=never";
        "catl" = "bat";
        "catn" = "cat --plain --paging=never";
      };
    };
  };
}
