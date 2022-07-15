
{ config, pkgs, ... }: 
let
  grubtheme = pkgs.fetchFromGitHub {
    owner = "cosasdepuma";
    repo = "virtuaverse-grub-theme";
    rev = "f3cb553c1949b1683a4ee5b0c1d665d29f5857eb";
    sha256 = "sha256-lCxOlH2U588l0BpTRW+TlfmyDDZLMQff/HtpbopmyA8=";
  };
in {
  imports = [ ../modules ];

  hardware.grub = {
    enable = true;
    supportEFI = true;
    theme = grubtheme;
  };  

  software = {
    extras = with pkgs;[ bunnyfetch dunst firefox ];

    alacritty.enable = true;
    bat.enable = true;
    coolretroterm.enable = true;
    feh.enable = true;
    fuck.enable = true;
    icons.enable = true;
    lightdm.enable = true;
    lsd.enable = true;
    pcmanfm.enable = true;
    picom.enable = true;
    rofi.enable = true;
    rofi.package = pkgs.rofi.override { plugins = with pkgs;[ rofi-calc rofi-emoji ]; };
    xmobar.enable = true;
    xmonad.enable = true;
    zsh.enable = true;
  };

  system = {
    languages = [ "es" ];
    timezone = "Europe/Madrid";
  };
}
