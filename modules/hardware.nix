{ config, lib, ... }:
with lib; {
  config = {
    # Sound
    sound.enable = mkDefault true;
    hardware.pulseaudio.enable = true;
    # Touchpad
    services.xserver.libinput.enable = mkDefault true;
  };
}