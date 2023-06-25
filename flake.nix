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
            curl = lib.mkEnableOption "cURL dotfiles";
            git  = lib.mkEnableOption "git dotfiles";
            ssh  = lib.mkEnableOption "SSH dotfiles";
            wget = lib.mkEnableOption "wget dotfiles";
          };

          config = lib.mkMerge [
            (lib.mkIf cfg.curl {
              home.packages = with pkgs; [ curl ];
              home.file = dotfile ".curlrc";
            })
            (lib.mkIf cfg.git {
              programs.git.enable = lib.mkDefault false;
              home.packages = with pkgs; [ git ];
              xdg.configFile = (dotconfig "git/config") // (dotconfig "git/ignore");
            })
            (lib.mkIf cfg.ssh {
              programs.ssh.enable = lib.mkDefault false;
              home.file = dotfile ".ssh/config";
            })
            (lib.mkIf cfg.wget {
              home.packages = with pkgs; [ wget ];
              home.file = dotfile ".wgetrc";
            })
          ];
        };
    };
}
