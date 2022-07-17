{ config, lib, pkgs, ... }:
let
  name = "lsd";
  cfg = config.software."${name}";
in {
  options = {
    software."${name}" = {
      enable = lib.mkEnableOption "custom ${name} (better ls)";

      package = lib.mkPackageOption pkgs name {};

      aliases = lib.mkOption {
        type = lib.types.attrs;
        default = {};
        example = { "cat" = "bat --paging=never"; };
        description = "Shell aliases for ${name}";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      shellAliases = cfg.aliases;
    };
  };
}
