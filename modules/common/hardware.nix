{ config, ... }: {
  config = {
    # Monitors
    services.autorandr.enable = true;
    # Bluetooth
    services.blueman.enable = true;
    # Sound
    sound.enable = mkDefault true;
    hardware.pulseaudio.enable = true;
    # Touchpad
    services.xserver.libinput.enable = true;
  };
}
