{ config, lib, pkgs, ... }:
let
  cfg = config.system.distribution;
in {
  options = {
    system.distribution = {
      name = lib.mkOption {
        type = lib.types.nullOr lib.types.nonEmptyStr;
        default = null;
        example = "B1O5";
        description = "Operative system name.";
      };
      version = lib.mkOption {
        type = lib.types.nonEmptyStr;
        default = "22.11 (Raccoon)";
        example = "(Boring)";
        description = "Operative system version.";
      };
    };
  };

  config = lib.mkIf (cfg.name != null) {
    system.activationScripts = lib.mkMerge [
      ({
        "os-release" = ''
          ${pkgs.gnused}/bin/sed -i -E 's/^(NAME)=.*$/\1="${cfg.name}"/g' /etc/os-release
          ${pkgs.gnused}/bin/sed -i -E 's/^(VERSION)=.*$/\1="${cfg.version}"/g' /etc/os-release
          ${pkgs.gnused}/bin/sed -i -E 's/^(PRETTY_NAME)=.*$/\1="${cfg.name} ${cfg.version}"/g' /etc/os-release
        '';
        "lsb-release" = ''
          ${pkgs.gnused}/bin/sed -i -E 's/^(DISTRIB_DESCRIPTION)=.*$/\1="${cfg.name} ${cfg.version}"/g' /etc/lsb-release
        '';
      })
      (lib.mkIf config.boot.loader.grub.enable {
        "grub-menuentry" = ''
          ${pkgs.gnused}/bin/sed -i -E 's/^((sub)?menu(entry)? )"[^\-]*-/\1 "B1O5 -/g' /boot/grub/grub.cfg
        '';
      })
    ];
  };
}
