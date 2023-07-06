{
  description = "Pumita's dotfiles";

  outputs = { self, nixpkgs }: {
    homeManagerModules.default = { config, lib, pkgs, ... }:
      let
        cfg = config.dotfiles;
      in with lib; {
        options.dotfiles.enable = mkEnableOption "dotfiles";

        config = mkIf cfg.enable {
          home.activation =
           let
              path = "/run/current-system/sw/bin";
           in {
            # cURL
            curl = lib.hm.dag.entryAfter ["writeBoundary"] ''
              ${pkgs.coreutils}/bin/test -x "${path}/curl" && ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.}/.curlrc ~/.curlrc
            '';
            # Local scripts
            local = lib.hm.dag.entryAfter ["writeBoundary"] ''
              ${pkgs.coreutils}/bin/mkdir -p ~/.local/bin
              ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.local/bin}/theme ~/.local/bin/theme
              command -v "feh"  >/dev/null && ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.local/bin}/feh  ~/.local/bin/feh
              command -v "swww" >/dev/null && ${pkgs.rsync}/bin/rsync -gortuxv --no-p ${./.local/bin}/swww ~/.local/bin/swww
              ${pkgs.coreutils}/bin/chmod -R +x ~/.local/bin/
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
