{ config, ... }: {
  config = {
    boot.loader.grub = {
      enable = true;
      device = "/dev/sda";
      useOSProber = true;
      version = 2;
    };
  };
}