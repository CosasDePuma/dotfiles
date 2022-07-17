{ config, lib, pkgs, ... }:
let
  name = "alacritty";
  cfg = config.software."${name}";
in {
  options = {
    software."${name}" = {
      enable = lib.mkEnableOption "custom ${name} (terminal emulator)";

      package = lib.mkPackageOption pkgs name {};

      config = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        example = ../../config/${name};
        description = "The path of the ${name} configuration file or folder.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      etc = lib.mkIf (cfg.config != null) {
        "${name}" = if (builtins.pathExists (builtins.toString cfg.config + "/.")) then {
          source = cfg.config;
          target = "xdg/${name}";
        } else {
          text = builtins.readConfig cfg.config;
          target = "xdg/${name}.yml";
        };
      };
    };
  };
}