{ config, lib, hostname ? "nixos", ... }:
let
  cfg = config.system.network;
in {
  options = {
    system.network = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        example = false;
        description = "Whether to enable network.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    networking.hostName = hostname;
  };
}
