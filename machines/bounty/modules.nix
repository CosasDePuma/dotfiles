{ pkgs, ... }: {
  imports = [ ../../modules ];

  base.languages.languages = ["us"];
  base.timezone = "Europe/Madrid";
  desktop.xmonad.enable = true;
  desktop.xmonad.defaultTerminal = pkgs.alacritty;
}