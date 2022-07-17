{ config, lib, pkgs, ... }:
let
  name = "bat";
  cfg = config.software."${name}";
in {
  options = {
    software."${name}" = {
      enable = lib.mkEnableOption "custom ${name} (better cat)";

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