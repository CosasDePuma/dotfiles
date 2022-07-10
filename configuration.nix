{ config, pkgs, ... }: {
  imports =
    [
      ./hardware-configuration.nix
      ./modules/wm/xmonad.nix
    ];


  networking.hostName = "bounty"; # Define your hostname.
  networking.networkmanager.enable = true;  # Enables wireless support via wpa_supplicant.
  time.timeZone = "Europe/Amsterdam";
  i18n.defaultLocale = "es_ES.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "es";
  };
  services.xserver.layout = "es";

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.pulseaudio.extraConfig = "unload-module module-suspend-on-idle";

  services.xserver.libinput.enable = true;

  users.users."bug" = {
    isNormalUser = true;
    extraGroups = [ "audio" "networkmanager" "wheel" ];
  };

  environment.systemPackages = with pkgs; [
    firefox
    vim
    wget
    xclip
    xorg.xrandr
  ];

  system.stateVersion = "21.11"; # Did you read the comment?

}