{ config, lib, pkgs, ... }:
let
  cfg = config.software.htop;
in {
  options = {
    software.htop = {
      enable = lib.mkEnableOption "custom htop (resources monitoring)";
      package = lib.mkPackageOption pkgs "htop" {};
      config = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        example = ../../config/htop/htoprc;
        description = "The path of the htop configuration file.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      etc."htop/htoprc" = lib.mkIf (cfg.config != null) {
        text = builtins.readFile cfg.config;
      };
    };
  };
}
