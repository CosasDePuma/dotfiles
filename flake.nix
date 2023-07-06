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
              if command -v "curl" >/dev/null; then
                ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.}/.curlrc ~/.curlrc
              fi
            '';
            # Local scripts
            local = lib.hm.dag.entryAfter ["writeBoundary"] ''
              mkdir -p ~/.local/bin
              ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.local/bin}/theme ~/.local/bin/theme
              command -v "feh"  >/dev/null && ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.local/bin}/feh  ~/.local/bin/feh
              command -v "swww" >/dev/null && ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.local/bin}/swww ~/.local/bin/swww
              chmod -R +x ~/.local/bin/
            '';
            # SSH
            ssh = lib.hm.dag.entryAfter ["writeBoundary"] ''
              mkdir -p ~/.ssh
              command -v "ssh" > /dev/null && ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.ssh}/. ~/.ssh/
            '';
            # Wget
            wget = lib.hm.dag.entryAfter ["writeBoundary"] ''
              command -v "wget" > /dev/null && ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.}/.wgetrc ~/.wgetrc
            '';
            # XMonad
            xmonad = lib.hm.dag.entryAfter ["writeBoundary"] ''
              mkdir -p ~/.config/xmonad
              command -v "xmonad" > /dev/null && ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.config/xmonad}/. ~/.config/xmonad/
            '';
          };
        };
      };
  };
}
