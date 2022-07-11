{ config, lib, ... }:
let
  cfg = config.base.sound;
in {
  options = {
    base.sound.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      example = false;
      description = "Whether to enable sound.";
    };
  };

  config = lib.mkIf cfg.enable {
    sound.enable = true;
    hardware.pulseaudio = {
      enable = true;
      support32Bit = true;
      extraConfig = "unload-module module-suspend-on-idle";
      extraClientConf = "autospawn=yes";
    };
  };
}