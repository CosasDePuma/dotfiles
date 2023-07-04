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
       in {
          options."${name}" = {
            all = lib.mkEnableOption "all the dotfiles";
            curl = lib.mkEnableOption "cURL dotfiles";
            git  = lib.mkEnableOption "git dotfiles";
            hypr = lib.mkEnableOption "Hyprland dotfiles";
            ssh  = lib.mkEnableOption "SSH dotfiles";
            swww  = lib.mkEnableOption "swww dotfiles";
            wget = lib.mkEnableOption "wget dotfiles";
          };

          config = lib.mkMerge [
            (lib.mkIf (cfg.all || cfg.curl) {
              home.packages = with pkgs; [ curl ];
              home.file = dotfile ".curlrc" false;
            })
            (lib.mkIf (cfg.all || cfg.git) {
              programs.git.enable = lib.mkDefault false;
              home.packages = with pkgs; [ git ];
              xdg.configFile =  (dotconfig "git/config" false) //
                                (dotconfig "git/ignore" false);
            })
            (lib.mkIf (cfg.all || cfg.hypr) {
              programs.git.enable = lib.mkDefault false;
              xdg.configFile =  (dotconfig "hypr/hyprland.conf" false) //
                                (dotconfig "hypr/bindings.conf" false) //
                                (dotconfig "hypr/themes/catppuccin.conf" false);
              
            })
            (lib.mkIf (cfg.all || cfg.ssh) {
              home.file = dotfile ".ssh/config" false;
            })
            (lib.mkIf (cfg.all || cfg.swww) {
              home.packages = with pkgs; [ swww ];
              xdg.configFile = dotconfig "swww" true;
              xdg.configFile."wallpapers" = { recursive = true; source = ./config/wallpapers; };
            })
            (lib.mkIf (cfg.all || cfg.wget) {
              home.packages = with pkgs; [ wget ];
              home.file = dotfile ".wgetrc" false;
            })
          ];
        };
    };
}
