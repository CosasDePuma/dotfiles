{ config, pkgs, ... }: {
  config = {
    # Environment
    services.xserver = {
      enable = true;
      displayManager.defaultSession = "none+i3";
      displayManager.lightdm.enable = true;
      windowManager.i3.enable = true;
      windowManager.i3.package = pkgs.i3-gaps;
    };
  
    # Programs
    environment.systemPackages = with pkgs; [
      feh            # wallpaper
      kitty          # terminal
      picom          # compositor
      rofi           # launcher
      dracula-theme  # theme
    ];
  };
}