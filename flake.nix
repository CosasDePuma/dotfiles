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
              if ${pkgs.coreutils}/bin/test -x "${path}/curl"; then
                $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.}/.curlrc ~/.curlrc
                $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG -R ug+rw,o-rwx ~/.curlrc
              fi
            '';
            # SSH
            ssh = lib.hm.dag.entryAfter ["writeBoundary"] ''
              if ${pkgs.coreutils}/bin/test -x "${path}/ssh"; then
                $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir $VERBOSE_ARG -p ~/.ssh/
                $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.}/.ssh/. ~/.ssh
                $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG -R ug+rw,o-rwx ~/.ssh/
              fi
            '';
            # Wget
            wget = lib.hm.dag.entryAfter ["writeBoundary"] ''
              if ${pkgs.coreutils}/bin/test -x "${path}/wget"; then
                $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.}/.wgetrc ~/.wgetrc
                $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG -R ug+rw,o-rwx ~/.wgetrc
              fi
            '';
            # XMonad
            xmonad = lib.hm.dag.entryAfter ["writeBoundary"] ''
              if ${pkgs.coreutils}/bin/test -x "${path}/xmonad"; then
                $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir $VERBOSE_ARG -p ~/.config/xmonad/
                $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.}/.config/xmonad/. ~/.config/xmonad
                $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG -R ug+rw,o-rwx ~/.config/xmonad/
              fi
            '';

            # Local scripts
            local = lib.hm.dag.entryAfter ["writeBoundary"] ''
              $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir $VERBOSE_ARG -p ~/.local/bin/
              $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.local/bin}/theme ~/.local/bin/theme

              # Feh
              if ${pkgs.coreutils}/bin/test -x "${path}/feh"; then
                $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.local/bin}/feh ~/.local/bin/feh
              fi

              # Swww
              if ${pkgs.coreutils}/bin/test -x "${path}/swww"; then
                $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.local/bin}/swww ~/.local/bin/swww
              fi
              $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG -R ug+rwx,o-x ~/.local/bin/
            '';
          };
        };
      };
  };
}
