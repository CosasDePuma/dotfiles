{ config, lib, ... }:
with lib; {
  config = {
    console.keyMap = mkDefault "es";
    i18n.defaultLocale = mkDefault "es_ES.UTF-8";
    time.timeZone = "Europe/Madrid";
    services.xserver.layout = mkDefault "es,us";
    services.xserver.xkbOptions = mkDefault "grp:alt_shift_toggle";
  };
}