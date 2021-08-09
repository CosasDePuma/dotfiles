{ config, ... }: {
  config = {
    networking = {
      networkmanager.enable = true;
      useDHCP = false;
      interfaces.eth0.useDHCP = true;
      wireless.userControlled.enable = true;
      usePredictableInterfaceNames = true;
      nameservers = [ "1.1.1.1" "8.8.8.8" ];
    };
  };
}