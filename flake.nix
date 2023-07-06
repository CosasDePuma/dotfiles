{
  description = "Pumita's dotfiles";

  outputs = { self, nixpkgs }: {
      homeManagerModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config.dotfiles;

          cpy = program: path: {
            "${program}" = lib.hm.dag.entryAfter ["writeBoundary"] ''
              # copy dotfiles to a directory
              if command -v "${program}" > /dev/null || test -z "${program}"; then
                $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync -gortux --no-p "${path}" ~/"${path}"
              fi
              # make scripts executable
              test "${program}" == "sh" && $DRY_RUN_CMD chmod -R +x ~/"${path}"
            '';
          };
       in with lib; {
          options.dotfiles.enable = mkEnableOption "dotfiles";

          config = mkIf cfg.enable {
            home.activation = mkMerge [
              (cpy "sh"     .local/bin)
              (cpy "ssh"    .ssh)
              (cpy "xmonad" .config/xmonad)
            ];
          };
        };
    };
}
