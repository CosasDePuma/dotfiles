{ config, pkgs, ... }: {
  imports = [
    ../../modules/wm/xmonad.nix
  ];

  networking.networkmanager.enable = true;
  time.timeZone = "Europe/Amsterdam";
  i18n.defaultLocale = "es_ES.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "es";
  };
  services.xserver.layout = "es,us";
  services.xserver.xkbOptions = "grp:lalt_lshift_toggle";

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.pulseaudio.extraConfig = "unload-module module-suspend-on-idle";

  services.xserver.libinput.enable = true;

  environment.systemPackages = with pkgs; [
    firefox
    git
    vim
    wget
    xclip
  ];
}
