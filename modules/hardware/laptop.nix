{ config, lib, ... }:
let
  cfg = config.hardware.laptop;
in {
  options = {
    hardware.laptop.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      example = false;
      description = "Whether to enable laptop support.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.xserver.libinput.enable = true;
  };
}
