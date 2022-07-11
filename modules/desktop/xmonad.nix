{ config, lib, pkgs, ... }:
let
  cfg = config.desktop.xmonad;
  extraPkgs = with pkgs;[ dunst feh rofi ];
in {
  options = {
    desktop.xmonad = {
      enable = lib.mkEnableOption "custom XMonad (with XMobar) window manager";
      defaultTerminal = lib.mkPackageOption pkgs "xterm" {};
      disableExtraPkgs = lib.mkEnableOption "disable extra packages";
      disableExtraFonts = lib.mkEnableOption "disable extra fonts";
    };
  };

  config = lib.mkIf cfg.enable {
    services.xserver = {
      enable = true;
      autorun = true;
      displayManager.defaultSession = "none+xmonad";
      windowManager.xmonad = {
        enable = true;
        config = builtins.replaceStrings ["xterm"] [cfg.defaultTerminal.pname] (builtins.readFile ../../config/xmonad/xmonad.hs);
        enableContribAndExtras = true;
      };
    };

    fonts.fonts = lib.optionals (!cfg.disableExtraFonts)  [ (pkgs.nerdfonts.override { fonts = [ "Mononoki" ]; }) ];

    environment.systemPackages = with pkgs;[ haskellPackages.xmobar xorg.xmessage ] ++ [ cfg.defaultTerminal ] ++ (lib.optionals (!cfg.disableExtraPkgs) extraPkgs);
  };
}