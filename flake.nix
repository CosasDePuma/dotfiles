{
  description = "Pumita's dotfiles";

  outputs = { self, nixpkgs }:
    let name = "dotfiles";
    in {
      homeManagerModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config."${name}";
          dotfile = path: x: writeFile path path x;
          dotconfig = path: x: writeFile ".config/${path}" path x;
          
          writeFile = src: dest: x: with lib.strings; {
            "${dest}" = {
              text = removePrefix "\n" (removeSuffix "\n" (builtins.readFile ./${src}));
              executable = x;
            };
          };
       in with lib; {
          options."${name}" = {
            all = mkEnableOption "all the dotfiles";
            curl = mkEnableOption "cURL dotfiles";
            git  = mkEnableOption "git dotfiles";
            hypr = mkEnableOption "Hyprland dotfiles";
            ssh  = mkEnableOption "SSH dotfiles";
            swww  = mkEnableOption "swww dotfiles";
            wallpapers = mkEnableOption "wallpapers";
            wget = mkEnableOption "wget dotfiles";
          };

          config = mkMerge [
            (mkIf (cfg.all || cfg.curl) {
              home.packages = with pkgs; [ curl ];
              home.file = dotfile ".curlrc" false;
            })
            (mkIf (cfg.all || cfg.git) {
              programs.git.enable = mkDefault false;
              home.packages = with pkgs; [ git ];
              xdg.configFile =  (dotconfig "git/config" false) //
                                (dotconfig "git/ignore" false);
            })
            (mkIf (cfg.all || cfg.hypr) {
              programs.git.enable = mkDefault false;
              xdg.configFile =  (dotconfig "hypr/hyprland.conf" false) //
                                (dotconfig "hypr/bindings.conf" false) //
                                (dotconfig "hypr/themes/catppuccin.conf" false);
              
            })
            (mkIf (cfg.all || cfg.ssh) {
              home.file = dotfile ".ssh/config" false;
            })
            (mkIf (cfg.all || cfg.swww) {
              home.packages = with pkgs; [ swww ];
              xdg.configFile = dotconfig "swww" true;
            })
            (mkIf (cfg.all || cfg.wallpapers) {
              xdg.configFile."wallpapers" = {
                recursive = true;
                source = .config/wallpapers;
              };
            })
            (mkIf (cfg.all || cfg.wget) {
              home.packages = with pkgs; [ wget ];
              home.file = dotfile ".wgetrc" false;
            })
          ];
        };
    };
}
