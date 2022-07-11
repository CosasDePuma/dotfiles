{ config, pkgs, ... }: {
  imports = [ ../../modules ];

  base.languages = [ "es" "us" ];
  base.timezone = "Europe/Madrid";
  desktop.picom.enable = true;
  desktop.xmonad.enable = true;
  desktop.xmonad.defaultTerminal = pkgs.alacritty;
}
