

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
    extras = with pkgs;[ bunnyfetch firefox ];

    alacritty = {
      enable = true;
      config = ../config/alacritty;
    };

    bat.enable = true;
    coolretroterm.enable = true;

    dunst = {
      enable = true;
      config = ../config/dunst;
    };

    feh = {
      enable = true;
      wallpapers = ../config/wallpapers/ghiblike.png;
    };

    htop.enable = true;

    lightdm = {
      enable = true;
      background = ../config/wallpapers/cryptogirl.jpg;
    };

    lsd.enable = true;
    pcmanfm.enable = true;

    picom = {
      enable = true;
      config = ../config/picom;
    };

    qemu.enable = true;

    rofi = {
      enable = true;
      package = pkgs.rofi.override { plugins = with pkgs;[ rofi-calc rofi-emoji ]; };
      config = ../config/rofi;
    };    

    thefuck.enable = true;

    xmobar = {
      enable = true;
      config = ../config/xmobar;
    };

    xmonad = {
      enable = true;
      config = ../config/xmonad/xmonad.hs;
    };

    zsh = {
      enable = true;
      config = ../config/zsh/zshrc;
    };
  };

  system = {
    appearance = {
      themes = [ pkgs.theme-vertex ];
      icons = [ pkgs.gnome.adwaita-icon-theme];
      fonts = [ (pkgs.nerdfonts.override { fonts = [ "Mononoki" ]; }) ];
      config = ../config/gtk-3.0;
    };

    distribution = {
      name = "B1O5";
      version = "(Boring)";
    };

    languages = [ "es" ];
    timezone = "Europe/Madrid";

    users = {
      "puma" = {
        sudo = true;
        description = "Pumita";
        groups = [ "networkmanager" ];
      };
    };
  };
}