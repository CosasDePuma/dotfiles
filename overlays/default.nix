
{ pkgs, ... }:
 builtins.map (overlay: import ./${overlay} { inherit pkgs; })
   (builtins.filter (x: x != "default.nix") (builtins.attrNames (builtins.readDir ./.)))
