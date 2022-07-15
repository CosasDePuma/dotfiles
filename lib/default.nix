{ nixpkgs, system ? "x86_64-linux", nixosVersion ? "22.05", ... }:
  nixpkgs.lib.foldl (x: y: x // y) {}
    (builtins.map (lib: import ./${lib} { inherit nixpkgs system nixosVersion; })
      (builtins.filter (x: x != "default.nix") (builtins.attrNames (builtins.readDir ./.))))
