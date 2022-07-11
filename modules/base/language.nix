{ config, lib, ... }:
let
  cfg = config.base;
  primary = builtins.elemAt cfg.languages 0;
in {
  options = {
    base = {
      languages = lib.mkOption {
        type = lib.types.nonEmptyListOf (lib.types.strMatching "^(es|us)$");
        default = ["us"];
        description = "Languages used by the system. The first item will be the default language. Available options: \"es\", \"us\".";
      };

      toggleLanguage = lib.mkOption {
        type = lib.types.nonEmptyStr;
        default = "lalt_lshift_toggle";
        description = "Keybindings for toggling the language (grp:xkbOptions format).";
      };

      timezone = lib.mkOption {
        type = lib.types.nonEmptyStr;
        default = "Europe/Amsterdam";
        description = "Timezone used by the system. See: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones";
      };
    };
  };

  config = {
  
    time.timeZone = cfg.timezone;
    i18n.defaultLocale = if primary == "es" then "es_ES.UTF-8" else "en_US.UTF-8";
    console = {
      keyMap = primary;
      font = "Lat2-Terminus16";
    };
    services.xserver.layout = lib.concatStringsSep "," cfg.languages;
    services.xserver.xkbOptions = "grp:${cfg.toggleLanguage}";
  
  };
}