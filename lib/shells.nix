
{ lib, pkgs, ... }: rec {
  # getShells :: list
  #   get all the named shells under a directory (without extension)
  getShells = builtins.map (x: lib.removeSuffix ".nix" x)
    (builtins.attrNames
      (lib.filterAttrs (_: v: v == "regular")
        (builtins.readDir ../shells)));

  # mkShells :: attrs
  #   generates multiple develop shells
  mkShells = lib.genAttrs getShells mkShell;

  # mkShell :: str -> attrs
  #   generates a develop shell using a nix file
  mkShell = name: import ../shells/${name}.nix { inherit pkgs; };
}
