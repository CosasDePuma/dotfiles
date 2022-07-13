{ config, pkgs, ... }: {
  imports = [ ../modules ];

  base.languages = [ "es" ];
  base.timezone = "Europe/Madrid";
  
  programs = {
    extras = with pkgs;[ bunnyfetch dunst firefox rofi ];
    alacritty.enable = true;
    feh.enable = true;
    picom.enable = true;
    xmobar.enable = true;
    xmonad.enable = true;
  };
}
