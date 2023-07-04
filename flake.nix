{
  description = "Pumita's dotfiles";

  outputs = { self, nixpkgs }:
    let name = "dotfiles";
    in {
      homeManagerModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config."${name}";
          dotfile = path: writeFile path path;
          dotconfig = path: writeFile ".config/${path}" path;
          
          writeFile = src: dest: with lib.strings; {
            "${dest}".text = removePrefix "\n" (removeSuffix "\n" (builtins.readFile ./${src}));
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
              home.file = dotfile ".curlrc";
            })
            (lib.mkIf (cfg.all || cfg.git) {
              programs.git.enable = lib.mkDefault false;
              home.packages = with pkgs; [ git ];
              xdg.configFile =  (dotconfig "git/config") //
                                (dotconfig "git/ignore");
            })
            (lib.mkIf (cfg.all || cfg.hypr) {
              programs.git.enable = lib.mkDefault false;
              xdg.configFile =  (dotconfig "hypr/hyprland.conf") //
                                (dotconfig "hypr/bindings.conf") //
                                (dotconfig "hypr/themes/catppuccin.conf");
              
            })
            (lib.mkIf (cfg.all || cfg.ssh) {
              programs.ssh.enable = lib.mkDefault false;
              home.file = dotfile ".ssh/config";
            })
            (lib.mkIf (cfg.all || cfg.swww) {
              home.packages = with pkgs; [ swww ];
              xdg.configFile = dotconfig "swww";
              xdg.configFile."swww".executable = true;
              xdg.configFile."wallpapers" = { recursive = true; source = ./config/wallpapers; };
            })
            (lib.mkIf (cfg.all || cfg.wget) {
              home.packages = with pkgs; [ wget ];
              home.file = dotfile ".wgetrc";
            })
          ];
        };
    };
}
