{ config, ... }: {
  config = {
    # Monitors
    services.autorand.enable = true;
    # Bluetooth
    services.blueman.enable = true;
    # Sound
    sound.enable = mkDefault true;
    hardware.pulseaudio.enable = true;
    # Touchpad
    services.xserver.libinput.enable = true;
  };
}