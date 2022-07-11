{ config, lib, pkgs, ... }:
let
  cfg = config.desktop.xmonad;
  extraPkgs = with pkgs;[ dunst feh rofi ];
in {
  options = {
    desktop.xmonad = {
      enable = lib.mkEnableOption "custom XMonad (with XMobar) window manager";

      config = lib.mkOption {
        type = lib.types.path;
        default = ../../config/xmonad/xmonad.hs;
        description = "The path of the XMonad configuration file. This file is embedded in the binary.";
      };

      defaultTerminal = lib.mkPackageOption pkgs "xterm" {};

      disableExtraFonts = lib.mkOption {
        type = lib.types.bool;
        default = false;
        example = true;
        description = "Whether to disable extra fonts";
      };

      disableExtraPackages = lib.mkOption {
        type = lib.types.bool;
        default = false;
        example = true;
        description = "Whether to disable extra packages";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.xserver = {
      enable = true;
      autorun = true;
      displayManager.defaultSession = "none+xmonad";
      windowManager.xmonad = {
        enable = true;
        config = builtins.replaceStrings ["xterm"] [cfg.defaultTerminal.pname] (builtins.readFile cfg.config);
        enableContribAndExtras = true;
      };
    };

    fonts.fonts = lib.optionals (!cfg.disableExtraFonts)  [ (pkgs.nerdfonts.override { fonts = [ "Mononoki" ]; }) ];

    environment.systemPackages = with pkgs;[ haskellPackages.xmobar xorg.xmessage ] ++ [ cfg.defaultTerminal ] ++ (lib.optionals (!cfg.disableExtraPackages) extraPkgs);
  };
}
