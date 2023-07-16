{
  description = "Pumita's dotfiles";

  outputs = { self, nixpkgs }: {
    homeManagerModules.default = { config, lib, pkgs, ... }:
      let
        cfg = config.dotfiles;
        sw-system = "/run/current-system/sw/bin";
        
        dotFile = pkg: path: {
          "${pkg}" = lib.hm.dag.entryAfter ["writeBoundary"] ''
            if ${pkgs.coreutils}/bin/test -x ${sw-system}/"${pkg}"; then
              $DRY_RUN_CMD ${pkgs.coreutils}/bin/rm $VERBOSE_ARG -f ~/${path}
              $DRY_RUN_CMD ${pkgs.coreutils}/bin/cp $VERBOSE_ARG -f ${./.}/${path} ~/${path}
              $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG ug+rw,o-rwx ~/${path}
            fi
          '';
        };
        dotFolder = pkg: path: {
          "${pkg}" = lib.hm.dag.entryAfter ["writeBoundary"] ''
            if ${pkgs.coreutils}/bin/test -x ${sw-system}/"${pkg}"; then
              $DRY_RUN_CMD ${pkgs.coreutils}/bin/rm $VERBOSE_ARG -rf ~/${path}
              $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir $VERBOSE_ARG -p ~/${path}
              $DRY_RUN_CMD ${pkgs.coreutils}/bin/cp $VERBOSE_ARG -rf ${./.}/${path}/. ~/${path}
              $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG -R ug+rw,o-rwx ~/${path}
            fi
          '';
        };
        cpyFolder = name: path: {
          "${name}" = lib.hm.dag.entryAfter ["writeBoundary"] ''
            $DRY_RUN_CMD ${pkgs.coreutils}/bin/rm $VERBOSE_ARG -rf ~/${path}
            $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir $VERBOSE_ARG -p ~/${path}
            $DRY_RUN_CMD ${pkgs.coreutils}/bin/cp $VERBOSE_ARG -rf ${./.}/${path} ~/${path}
            $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG -R ug+rw,o-rwx ~/${path}
          '';
        };

      in with lib; {
        options.dotfiles.enable = mkEnableOption "dotfiles";

        config = mkIf cfg.enable {
          home = {
            activation = mkMerge [
              (dotFile   "curl"             ".curlrc"                   )
              (dotFolder "Hyprland"         ".config/hypr"              )
              (dotFolder "kitty"            ".config/kitty"             )
              (dotFolder "neofetch"         ".config/neofetch"          )
              (dotFolder "picom"            ".config/picom"             )
              (dotFolder "ssh"              ".ssh"                      )
              (cpyFolder "theme-catppuccin" ".config/themes/catppuccin" )
              (cpyFolder "wallpapers"       ".config/wallpapers"        )
              (dotFile   "wget"             ".wgetrc"                   )
              (dotFolder "xmonad"           ".xmonad"                   )

              ({
                local-scripts = hm.dag.entryAfter ["writeBoundary"] ''
                  $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir $VERBOSE_ARG -p ~/.local/bin/
                  $DRY_RUN_CMD ${pkgs.coreutils}/bin/rm $VERBOSE_ARG -f ~/.local/bin/theme
                  $DRY_RUN_CMD ${pkgs.coreutils}/bin/cp $VERBOSE_ARG -f ${./.}/.local/bin/theme ~/.local/bin/theme

                  if ${pkgs.coreutils}/bin/test -x ${sw-system}/feh; then
                    $DRY_RUN_CMD ${pkgs.coreutils}/bin/rm $VERBOSE_ARG -f ~/.local/bin/feh
                    $DRY_RUN_CMD ${pkgs.coreutils}/bin/cp $VERBOSE_ARG -f ${./.}/.local/bin/feh ~/.local/bin/feh
                  fi

                  if ${pkgs.coreutils}/bin/test -x ${sw-system}/swww; then
                    $DRY_RUN_CMD ${pkgs.coreutils}/bin/rm $VERBOSE_ARG -f ~/.local/bin/swww
                    $DRY_RUN_CMD ${pkgs.coreutils}/bin/cp $VERBOSE_ARG -f ${./.}/.local/bin/swww ~/.local/bin/swww
                  fi

                  $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod $VERBOSE_ARG -R ug+rwx,o-x ~/.local/bin/
                '';
              })
            ];

            sessionVariables = {
              PATH = "$PATH:$HOME/.local/bin";
            };
          };
        };
      };
  };
}
