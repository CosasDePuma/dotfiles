{
  description = "Pumita's dotfiles";

  outputs = { self, nixpkgs }: {
    homeManagerModules.default = { config, lib, pkgs, ... }:
      let
        cfg = config.dotfiles;
      in with lib; {
        options.dotfiles.enable = mkEnableOption "dotfiles";

        config = mkIf cfg.enable {
          home.activation = {
            # Local scripts
            "bin" = lib.hm.dag.entryAfter ["writeBoundary"] ''
              $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.local/bin} ~/.local/bin
            '';
            # SSH
            "ssh" = lib.hm.dag.entryAfter ["writeBoundary"] ''
              command -v "ssh" > /dev/null && $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.ssh} ~/.ssh
            '';
          };
        };
      };
  };
}
