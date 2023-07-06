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
            # cURL
            curl = lib.hm.dag.entryAfter ["writeBoundary"] ''
              command -v "curl" > /dev/null && $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.}/.curlrc ~/.curlrc
            '';
            # Local scripts
            local = lib.hm.dag.entryAfter ["writeBoundary"] ''
              $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.local/bin}/theme ~/.local/bin/theme
              command -v "feh"  >/dev/null && $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.local/bin}/feh  ~/.local/bin/feh
              command -v "swww" >/dev/null && $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.local/bin}/swww ~/.local/bin/swww
              $DRY_RUN_CMD chmod -R +x ~/.local/bin/
            '';
            # SSH
            ssh = lib.hm.dag.entryAfter ["writeBoundary"] ''
              command -v "ssh" > /dev/null && $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.ssh}/. ~/.ssh/
            '';
            # Wget
            wget = lib.hm.dag.entryAfter ["writeBoundary"] ''
              command -v "wget" > /dev/null && $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.}/.wgetrc ~/.wgetrc
            '';
            # XMonad
            xmonad = lib.hm.dag.entryAfter ["writeBoundary"] ''
              command -v "xmonad" > /dev/null && $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.config/xmonad}/. ~/.config/xmonad/
            '';
          };
        };
      };
  };
}
