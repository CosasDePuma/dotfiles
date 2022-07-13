{ config, lib, pkgs, ... }:
let
  cfg = config.programs;
  defaults = with pkgs;[ curl git gnumake nano wget xclip ];
  extras   = with pkgs;[ firefox ];
in {
  imports = [
    ./alacritty.nix
    ./feh.nix
    ./picom.nix
    ./xmobar.nix
    ./xmonad.nix
  ];

  options.programs = {
    defaults.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      example = false;
      description = "Whether to enable default packages.";
    };

    recommended = {
      enable = lib.mkEnableOption "recommended packages.";

      packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = extras;
        example = [ pkgs.qutebrowser ];
        description = "List of recommended packages.";
      };
    };

    extras = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [];
      example = [ pkgs.bunnyfetch ];
      description = "More packages to be installed.";
    };
  };

  config = {
    environment.systemPackages = (lib.optionals cfg.defaults.enable defaults)
      ++ (lib.optionals cfg.recommended.enable cfg.recommended.packages)
      ++ cfg.extras;
  };
}
