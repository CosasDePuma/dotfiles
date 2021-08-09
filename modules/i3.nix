{ config, lib, pkgs, ... }:
with lib; {
  config = {
    # Environment
    services.xserver.enable = mkDefault true;
    services.xserver.displayManager.defaultSession = mkDefault "none+i3";
    services.xserver.displayManager.lightdm.enable = mkDefault true;
    services.xserver.windowManager.i3.enable = mkDefault true;
    services.xserver.windowManager.i3.package = mkDefault pkgs.i3-gaps;
    # Programs
    environment.systemPackages = with pkgs; mkDefault [
      feh            # wallpaper
      kitty          # terminal
      picom          # compositor
      rofi           # launcher
      dracula-theme  # theme
    ];
  };
}