{
  description = "Pumita's dotfiles";

  outputs = { self, nixpkgs }: {
    homeManagerModules.default = { config, lib, pkgs, ... }:
      let
        cfg = config.dotfiles;
        sw-profile = "~/.nix-profile/bin";
        sw-system = "/run/current-system/sw/bin";
        
        cpyFile = pkg: path: {
          "${pkg}" = lib.hm.dag.entryAfter ["writeBoundary"] ''
            if ${pkgs.coreutils}/bin/test -x "${sw-profile}/${pkg}" || ${pkgs.coreutils}/bin/test -x "${sw-system}/${pkg}"; then
              $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.}/${path} ~/${path}
              $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG ug+rw,o-rwx ~/${path}
            fi
          '';
        };
        cpyFolder = pkg: path: {
          "${pkg}" = lib.hm.dag.entryAfter ["writeBoundary"] ''
            if ${pkgs.coreutils}/bin/test -x "${sw-profile}/${pkg}" || ${pkgs.coreutils}/bin/test -x "${sw-system}/${pkg}"; then
              $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir $VERBOSE_ARG -p ~/${path}
              $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.}/${path}/. ~/${path}
              $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG -R ug+rw,o-rwx ~/${path}
            fi
          '';
        };

      in with lib; {
        options.dotfiles.enable = mkEnableOption "dotfiles";

        config = mkIf cfg.enable {
          home.activation = mkMerge [
            (cpyFile   "curl"     ".curlrc"       )
            (cpyFolder "ssh"      ".ssh"          )
            (cpyFile   "wget"     ".wgetrc"       )
            (cpyFolder "xmonad"   ".config/xmonad")

            ({
              local-scripts = lib.hm.dag.entryAfter ["writeBoundary"] ''
                $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir $VERBOSE_ARG -p ~/.local/bin/
                $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.}/.local/bin/theme ~/.local/bin/theme

                if ${pkgs.coreutils}/bin/test -x "${sw-profile}/feh" || ${pkgs.coreutils}/bin/test -x "${sw-system}/feh"; then
                  $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.}/.local/bin/feh ~/.local/bin/feh
                fi

                if ${pkgs.coreutils}/bin/test -x "${sw-profile}/swww" || ${pkgs.coreutils}/bin/test -x "${sw-system}/swww"; then
                  $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.}/.local/bin/swww ~/.local/bin/swww
                fi

                $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG -R ug+rwx,o-x ~/.local/bin/
              '';
            })
          ];
        };
      };
  };
}
