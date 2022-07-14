{ config, lib, pkgs, ... }:
let
  cfg = config.software;
  defaults = with pkgs;[ curl git gnumake nano wget xclip ];
  extras   = with pkgs;[ firefox ];
in {
  imports = [
    ./alacritty.nix
    ./feh.nix
    ./icons.nix
    ./picom.nix
    ./rofi.nix
    ./xmobar.nix
    ./xmonad.nix
    ./zsh.nix
  ];

  options.software = {
    defaults.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      example = false;
      description = "Whether to enable default software.";
    };

    recommended = {
      enable = lib.mkEnableOption "recommended software.";

      packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = extras;
        example = [ pkgs.qutebrowser ];
        description = "List of recommended software.";
      };
    };

    extras = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [];
      example = [ pkgs.bunnyfetch ];
      description = "More software to be installed.";
    };
  };

  config = {
    environment.systemPackages = (lib.optionals cfg.defaults.enable defaults)
      ++ (lib.optionals cfg.recommended.enable cfg.recommended.packages)
      ++ cfg.extras;
  };
}
