{ config, lib, ... }:
let
  cfg = config.software.fuck;
in {
  options = {
    software.fuck = {
      enable = lib.mkEnableOption "custom thefuck (command suggester)";
    };
  };

  config = {
    programs.thefuck.enable = cfg.enable;
  };
}
