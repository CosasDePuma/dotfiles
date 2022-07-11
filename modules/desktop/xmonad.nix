{ config, lib, pkgs, ... }:
let
  cfg = config.desktop.xmonad;
in {

  options = {
    desktop.xmonad = {
      enable = lib.mkEnableOption "custom XMonad (with XMobar)";
      
      defaultTerminal = lib.mkPackageOption pkgs "xterm" {};
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

    fonts.fonts = with pkgs; [ nerdfonts ];

    environment.systemPackages = with pkgs; [
      feh
      dunst
      rofi
      xorg.xmessage
      haskellPackages.xmobar
    ] ++ [ cfg.defaultTerminal ];
  };
}