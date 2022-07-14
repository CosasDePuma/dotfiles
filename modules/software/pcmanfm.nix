{ config, lib, pkgs, ... }:
let
  cfg = config.software.pcmanfm;
in {
  options = {
    software.pcmanfm = {
      enable = lib.mkEnableOption "custom pcmanfm (file manager)";

      package = lib.mkPackageOption pkgs "pcmanfm" {};

      theme = {
        package = lib.mkOption {
          type = lib.types.nullOr lib.types.package;
          default = null;
          example = pkgs.gruvbox-dark-gtk;
          description = "GTK theme package to install";
        };

        name = lib.mkOption {
          type = lib.types.nullOr lib.types.nonEmptyStr;
          default = null;
          example = "gruvbox-dark";
          description = "GTK theme name exported to GTK_THEME env variable.";
        };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ] ++ (lib.optional (cfg.theme.package != null) cfg.theme.package);
      variables = lib.mkIf (lib.all (x: x != null) [ cfg.theme.name cfg.theme.package]) {
        "GTK_THEME" = cfg.theme.name;
      };
    };
  };
}
