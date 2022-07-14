{ config, lib, pkgs, ... }:
let
  cfg = config.software.xmobar;
in {
  options = {
    software.xmobar = {
      enable = lib.mkEnableOption "custom XMobar (status bar)";

      package = lib.mkPackageOption pkgs.haskellPackages "xmobar" {};

      fonts = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ (pkgs.nerdfonts.override { fonts = [ "Mononoki" ]; }) ];
        example = [];
        description = "Extra fonts to install.";
      };

      config = lib.mkOption {
        type = lib.types.path;
        default = ../../config/xmobar/xmobarrc;
        description = "The path of the XMobar configuration file.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      etc."xmobar/xmobarrc".text = builtins.readFile cfg.config;
    };
    fonts.fonts = cfg.fonts;
  };
}
