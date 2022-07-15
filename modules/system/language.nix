{ config, lib, ... }:
let
  cfg = config.system;
  locales = { es = "es_ES"; us = "en_US"; };
  languages = builtins.concatStringsSep "|" (builtins.attrNames locales);
  genLocale = locale: (builtins.getAttr locale locales) + ".UTF-8";
  primary = rec { language = builtins.elemAt cfg.languages 0; locale = genLocale primary.language; };
in {
  options = {
    system = {
      languages = lib.mkOption {
        type = lib.types.nonEmptyListOf (lib.types.strMatching "^(${languages})$");
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
    console.keyMap = primary.language;
    time.timeZone = cfg.timezone;

    i18n = {
      defaultLocale = primary.locale;
      supportedLocales = builtins.map (x: (genLocale x) + "/UTF-8") cfg.languages;
      extraLocaleSettings = {
        "LC_MEASUREMENT" = primary.locale;
        "LC_MESSAGES" = primary.locale;
        "LC_MONETARY" = primary.locale;
        "LC_NUMERIC" = primary.locale;
        "LC_TIME" = primary.locale;
      };
    };

    services.xserver = {
      layout = lib.concatStringsSep "," cfg.languages;
      xkbOptions = "grp:${cfg.toggleLanguage}";
    };
  };
}
