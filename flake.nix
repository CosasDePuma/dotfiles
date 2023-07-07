{
  description = "Pumita's dotfiles";

  outputs = { self, nixpkgs }: {
    homeManagerModules.default = { config, lib, pkgs, ... }:
      let
        cfg = config.dotfiles;
        sw-system = "/run/current-system/sw/bin";
        
        dotFile = pkg: path: {
          "${pkg}" = lib.hm.dag.entryAfter ["reloadSystemd"] ''
            if ${pkgs.coreutils}/bin/test -x ${sw-system}/"${pkg}"; then
              $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.}/${path} ~/${path}
              $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG ug+rw,o-rwx ~/${path}
            fi
          '';
        };
        dotFolder = pkg: path: {
          "${pkg}" = lib.hm.dag.entryAfter ["reloadSystemd"] ''
            if ${pkgs.coreutils}/bin/test -x ${sw-system}/"${pkg}"; then
              $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir $VERBOSE_ARG -p ~/${path}
              $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.}/${path}/. ~/${path}
              $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG -R ug+rw,o-rwx ~/${path}
            fi
          '';
        };
        cpyFolder = name: path: {
          "${name}" = lib.hm.dag.entryAfter ["reloadSystemd"] ''
            $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir $VERBOSE_ARG -p ~/${path}
            $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.}/${path}/. ~/${path}
            $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG -R ug+rw,o-rwx ~/${path}
          '';
        };

      in with lib; {
        options.dotfiles.enable = mkEnableOption "dotfiles";

        config = mkIf cfg.enable {
          home.activation = mkMerge [
            (dotFile   "curl"             ".curlrc"                   )
            (dotFolder "Hyprland"         ".config/hypr"              )
            (dotFile   "kitty"            ".config/kitty"             )
            (dotFolder "neofetch"         ".config/neofetch"          )
            (dotFolder "ssh"              ".ssh"                      )
            (cpyFolder "theme-catppuccin" ".config/themes/catppuccin" )
            (dotFile   "wget"             ".wgetrc"                   )
            (dotFolder "xmonad"           ".xmonad"                   )
            (cpyFolder "wallpapers"       ".config/wallpapers"        )

            ({
              local-scripts = hm.dag.entryAfter ["reloadSystemd"] ''
                $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir $VERBOSE_ARG -p ~/.local/bin/
                $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.}/.local/bin/theme ~/.local/bin/theme

                if ${pkgs.coreutils}/bin/test -x ${sw-system}/"feh"; then
                  $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.}/.local/bin/feh ~/.local/bin/feh
                fi

                if ${pkgs.coreutils}/bin/test -x ${sw-system}/"swww"; then
                  $DRY_RUN_CMD ${pkgs.rsync}/bin/rsync $VERBOSE_ARG -gortux --no-p ${./.}/.local/bin/swww ~/.local/bin/swww
                fi

                $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG -R ug+rwx,o-x ~/.local/bin/
              '';

              postInstallation = hm.dag.entryAfter ["local-scripts"] ''
                $DRY_RUN_CMD ${pkgs.bash-interactive}/bin/bash ~/.local/bin/theme catppuccin
              ''
            })
          ];
        };
      };
  };
}
