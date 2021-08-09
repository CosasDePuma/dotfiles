{ config, lib, ... }:
with lib; {
  config = {
    boot.loader.grub.enable = mkDefault true;
    boot.loader.grub.device = mkDefault "/dev/sda";
    boot.loader.grub.useOSProber = true;
    boot.loader.grub.version = 2;
  };
}