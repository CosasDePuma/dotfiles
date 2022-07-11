{ config, lib, pkgs, ... }:
let
  cfg = config.base.defaultPackages;
in {
  options = {
    base.defaultPackages.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      example = false;
      description = "Whether to enable default packages.";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      firefox  # TODO make an own module
      git
      gnumake
      vim
      wget
      xclip
    ];
  };
}