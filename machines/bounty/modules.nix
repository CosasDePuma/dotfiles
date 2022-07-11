{ pkgs, ... }: {
  imports = [ ../../modules ];

  desktop.xmonad.enable = true;
  desktop.xmonad.defaultTerminal = pkgs.alacritty;
}