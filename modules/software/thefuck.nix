{ config, lib, ... }:
let
  name = "thefuck";
  cfg = config.software."${name}";
in {
  options = {
    software."${name}" = {
      enable = lib.mkEnableOption "custom ${name} (command suggester)";
    };
  };

  config = {
    programs.thefuck.enable = cfg.enable;
  };
}