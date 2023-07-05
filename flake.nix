{
  description = "Pumita's dotfiles";

  outputs = { self, nixpkgs }: {
      homeManagerModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config.dotfiles;
          binfile   = path: writeFile path path true;
          dotfile   = path: writeFile path path false;
          dotconfig = path: writeFile ".config/${path}" path false;
          
          writeFile = src: dest: x: with lib.strings; {
            "${dest}" = {
              text = removePrefix "\n" (removeSuffix "\n" (builtins.readFile ./${src}));
              executable = x;
            };
          };
       in with lib; {
          options.dotfiles = {
            all        = mkEnableOption "all the dotfiles";
            curl       = mkEnableOption "cURL dotfiles";
            foot       = mkEnableOption "foot dotfiles";
            git        = mkEnableOption "git dotfiles";
            hyprland   = mkEnableOption "Hyprland dotfiles";
            neofetch   = mkEnableOption "neofetch dotfiles";
            ssh        = mkEnableOption "SSH dotfiles";
            swww       = mkEnableOption "swww dotfiles";
            wallpapers = mkEnableOption "wallpapers";
            wget       = mkEnableOption "wget dotfiles";

            themes = {
              enable     = mkEnableOption "themes";
              catppuccin = mkEnableOption "catppuccin theme";
            };
          };

          config = mkMerge [
            # --------------------
            #   System utils
            # --------------------

            # cURL
            (mkIf (cfg.all || cfg.curl) {
              home.packages = with pkgs; [ curl ];
              home.file = dotfile ".curlrc";
            })
            # Git
            (mkIf (cfg.all || cfg.git) {
              programs.git.enable = mkDefault false;
              home.packages = with pkgs; [ git ];
              xdg.configFile = (dotconfig "git/config") //
                               (dotconfig "git/ignore");
            })
            # Neofetch
            (mkIf (cfg.all || cfg.neofetch) {
              home.packages = with pkgs; [ neofetch];
              xdg.configFile = dotconfig "neofetch/config.conf";
            })   
            # SSH
            (mkIf (cfg.all || cfg.ssh) {
              home.file = dotfile ".ssh/config";
            })            
            # Wget
            (mkIf (cfg.all || cfg.wget) {
              home.packages = with pkgs; [ wget ];
              home.file = dotfile ".wgetrc";
            })

            # --------------------
            #   WM/DE utils
            # --------------------

            # Foot
            (mkIf (cfg.all || cfg.git) {
              programs.git.enable = mkDefault false;
              home.packages = with pkgs; [ foot ];
              xdg.configFile = dotconfig "foot/foot.ini";
            })
            # Hyprland
            (mkIf (cfg.all || cfg.hyprland) {
              xdg.configFile = (dotconfig "hypr/hyprland.conf") //
                               (dotconfig "hypr/bindings.conf");
            })
            # Swww
            (mkIf (cfg.all || cfg.swww) {
              home.packages = with pkgs; [ swww ];
              home.file = binfile ".local/bin/swww";
            })

            # --------------------
            #   Themes
            # --------------------
            (mkIf (cfg.all || cfg.themes.enable) {
              home.file = binfile ".local/bin/theme";
              home.shellAliases."theme" = "~/.local/bin/theme ";
            })

            # catppuccin
            (mkIf (cfg.all || cfg.themes.catppuccin) {
              xdg.configFile."themes/catppuccin" = {
                recursive = true;
                source = .config/themes/catppuccin;
              };
            })

            # --------------------
            #   Wallpapers
            # --------------------
            (mkIf (cfg.all || cfg.wallpapers) {
              xdg.configFile."wallpapers" = {
                recursive = true;
                source = .config/wallpapers;
              };
            })
          ];
        };
    };
}
