{ config, lib, pkgs, ... }:
let
  cfg = config.software.zsh;
in {
  options = {
    software.zsh = {
      enable = lib.mkEnableOption "custom zsh (shell)";

      package = lib.mkPackageOption pkgs "zsh" {};

      config = lib.mkOption {
        type = lib.types.path;
        default = ../../config/zsh/zshrc;
        description = "The path of the zsh configuration file.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment = {
      systemPackages = [ cfg.package ];
      etc."zshrc".text = builtins.readFile cfg.config;
    };
    users.defaultUserShell = cfg.package;
  };
}
