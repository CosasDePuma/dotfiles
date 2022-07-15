{ nixpkgs, system ? "x86_64-linux", nixosVersion ? "22.05", ... }:
let
  args = (import nixpkgs { inherit system; }) // { inherit nixpkgs nixosVersion; };
in
  nixpkgs.lib.foldl (x: y: x // y) {}
    (builtins.map (lib: import ./${lib} args)
      (builtins.filter (x: x != "default.nix") (builtins.attrNames (builtins.readDir ./.))))
