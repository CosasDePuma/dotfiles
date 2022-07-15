
{ config, lib, pkgs, ... }:
let
  cfg = config.hardware.grub;
  themeFolder = "theme";
in {
  options = {
    hardware.grub = {
      enable = lib.mkEnableOption "custom GRUB2 (bootloader)";

      theme = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default = null;
        description = "GRUB2 theme to be installed.";
      };

      backgroundExt = lib.mkOption {
        type = lib.types.strMatching "^(png|jpe?g)$";
        default = "png";
        example = "jpg";
        description = "Extension of the /boot/${themeFolder} background file";
      };

      supportEFI = lib.mkEnableOption "support UEFI";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    ({
      boot.loader.grub = {
        enable = true;
        device = "nodev";
        fsIdentifier = "uuid";
        efiSupport = cfg.supportEFI;
        efiInstallAsRemovable = cfg.supportEFI;
      };
    })

    (lib.mkIf (cfg.theme != null) {
      boot.loader.grub = {
        extraConfig = "set theme=($drive1)//${themeFolder}/theme.txt";
        splashImage = "${cfg.theme}/background.${cfg.backgroundExt}";
      };
      system.activationScripts = {
        "grub-theme" = ''
          test -d /boot/${themeFolder} && rm -rf /boot/${themeFolder}
          cp -R ${cfg.theme} /boot/${themeFolder}
        '';
      };
    })
  ]);
}
