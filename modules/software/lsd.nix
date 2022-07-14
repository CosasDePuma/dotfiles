{ config, lib, pkgs, ... }:
let
  cfg = config.software.lsd;
in {
  options = {
    software.lsd = {
      enable = lib.mkEnableOption "custom lsd (better ls)";

      package = lib.mkPackageOption pkgs "lsd" {};
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      shellAliases = {
        "ls"  = "lsd";
        "la"  = "lsd -a";
        "ll"  = "lsd -l";
        "lla" = "lsd -la";
      };
    };
  };
}
