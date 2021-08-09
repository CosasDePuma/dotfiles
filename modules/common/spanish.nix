{ config, ... }:  {
  config = {
    console.keyMap = "es";
    i18n.defaultLocale = "es_ES.UTF-8";
    time.timeZone = "Europe/Madrid";
    services.xserver.layout = "es,us";
    services.xserver.xkbOptions = "grp:alt_shift_toggle";
  };
}