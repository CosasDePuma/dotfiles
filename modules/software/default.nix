{ config, lib, pkgs, ... }:
let
  cfg = config.software;
  defaults = with pkgs;[ curl git gnumake nano unzip wget xclip zip ];
  extras   = with pkgs;[ firefox ];
in {
  imports = builtins.map (x: ./${x})
    (builtins.attrNames
      (lib.filterAttrs (n: _: n != "default.nix")
        (builtins.readDir ./.)));

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