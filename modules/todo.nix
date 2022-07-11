{ config, lib, pkgs, ... }:
let
  cfg = config.experimental;
in {
  options = {
    experimental.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      example = false;
      description = "Whether to enable unsorted configurations.";
    };
  };

  config = lib.mkIf cfg.enable {
    networking.networkmanager.enable = true;
    services.xserver.libinput.enable = true;
    environment.systemPackages = with pkgs; [
      firefox
      git
      vim
      wget
      xclip
    ];
  };
}