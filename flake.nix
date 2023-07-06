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
            "bin" = lib.hm.dag.entryAfter ["writeBoundary"] ''
              $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync -gortux --no-p ${./.local/bin} ~/.local/bin
            '';

            "ssh" = lib.hm.dag.entryAfter ["writeBoundary"] ''
              command -v "ssh" > /dev/null && $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync -gortux --no-p ${./.ssh} ~/.ssh
            '';
          };
        };
      };
  }
}
