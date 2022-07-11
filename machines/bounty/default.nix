{ config, pkgs, ... }: {
  imports = [ ./modules.nix ];

  networking.networkmanager.enable = true;

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
