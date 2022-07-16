{ config, lib, pkgs, ... }:
let
  cfg = config.software.xmobar;
in {
  options = {
    software.xmobar = {
      enable = lib.mkEnableOption "custom XMobar (status bar)";

      package = lib.mkPackageOption pkgs.haskellPackages "xmobar" {};

      config = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        example = ../../config/xmobar/xmobarrc;
        description = "The path of the XMobar configuration file.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      etc."xmobar/xmobarrc" = lib.mkIf (cfg.config != null) {
        text = builtins.readFile cfg.config;
      };
    };
  };
}
