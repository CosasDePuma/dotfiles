

{ config, pkgs, ... }: {
  imports = [ ../modules ];

  hardware.grub.enable = true;

  software = {
    extras = with pkgs;[ firefox ];

    alacritty = { enable = true; config = ../config/alacritty; };
    mate.enable = true;
    lightdm = { enable = true; };
    lsd.enable = true;
    thefuck.enable = true;
    zsh = { enable = true; config = ../config/zsh/zshrc; };
  };

  system = {
    distribution = { name = "Mob1l3"; version = "(Android & iOS)"; };
    languages = [ "es" ];
    timezone = "Europe/Madrid";
    users = {
      "lab" = { sudo = true; description = "Mobile Playground"; groups = [ "networkmanager" ]; };
    };
  };
}
