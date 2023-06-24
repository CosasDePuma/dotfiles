{
  description = "Pumita's dotfiles";

  outputs = { self, nixpkgs }:
    let name = "dotfiles";
    in {
      homeManagerModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config."${name}";
          dotfile = path:
            with lib.strings; { "${path}".text = removePrefix "\n" (removeSuffix "\n" (builtins.readFile ./.config/${path})); };
       in {
          options."${name}" = {
            curl = lib.mkEnableOption "cURL dotfiles";
            git  = lib.mkEnableOption "git dotfiles";
            ssh  = lib.mkEnableOption "SSH dotfiles";
            wget = lib.mkEnableOption "wget dotfiles";
          };

          config =
            lib.mkIf cfg.curl {
              home.packages = with pkgs; [ curl ];
              xdg.configFile = dotfile "../.curlrc";
            } // lib.mkIf cfg.git {
              programs.git.enable = true;
              xdg.configFile = dotfile "git/config" // dotfile "git/ignore";
            } // lib.mkIf cfg.ssh {
              programs.ssh.enable = true;
              xdg.configFile = dotfile "ssh/config";
            } // lib.mkIf cfg.wget {
              home.packages = with pkgs; [ wget ];
              xdg.configFile = dotfile "../.wgetrc";
            };
        };
    };
}
