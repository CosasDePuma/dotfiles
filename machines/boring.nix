{ config, pkgs, ... }: {
  imports = [ ../modules ];

  base.languages = [ "es" ];
  base.timezone = "Europe/Madrid";
  
  software = {
    extras = with pkgs;[ bunnyfetch dunst firefox ];

    alacritty.enable = true;
    bat.enable = true;
    coolretroterm.enable = true;
    feh.enable = true;
    icons.enable = true;
    lsd.enable = true;
    pcmanfm.enable = true;
#   pcmanfm.theme.name = "gruvbox-dark"; # FIXME only for pcmanfm
    pcmanfm.theme.package = pkgs.gruvbox-dark-gtk;
    picom.enable = true;
    rofi.enable = true;
    rofi.package = pkgs.rofi.override { plugins = with pkgs;[ rofi-calc rofi-emoji ]; };
    xmobar.enable = true;
    xmonad.enable = true;
    zsh.enable = true;
  };
}
