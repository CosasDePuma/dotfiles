{
  description = "Pumita's dotfiles";

  outputs = { self, nixpkgs }:
    let name = "dotfiles";
    in {
      homeManagerModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config."${name}";
          dotfile = path:
            with lib.strings; { "${path}".text = removePrefix "\n" (removeSuffix "\n" (builtins.readFile ./${path})); };
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
              home.packages = with pkgs; [ git ];
              home.file = lib.mkMerge [
                (dotfile ".config/git/config")
                (dotfile ".config/git/ignore")
              ];
            })
            (lib.mkIf cfg.ssh {
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
