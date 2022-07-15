{ config, lib, pkgs, ... }:
let
  cfg = config.system.flakes;
in {
  options = {
    system.flakes.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      example = false;
      description = "Whether to enable nix flakes.";
    };
  };

  config = lib.mkIf cfg.enable {
    nix = {
      package = pkgs.nixFlakes;
      extraOptions = "experimental-features = nix-command flakes";
    };
  };
}
