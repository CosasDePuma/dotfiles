{ config, lib, ... }:
with lib; {
  config = {
    networking.networkmanager.enable = true;
    networking.useDHCP = false;
    networking.interfaces.eth0.useDHCP = mkDefault true;
    networking.interfaces.wlan0.useDHCP = mkDefault (!config.networking.interfaces.eth0.useDHCP);
    networking.wireless.userControlled.enable = true;
    networking.usePredictableInterfaceNames = mkDefault true;
    networking.nameservers = mkDefault [ "1.1.1.1" "8.8.8.8" ];
  };
}