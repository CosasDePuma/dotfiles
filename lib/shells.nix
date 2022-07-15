
{ nixpkgs, ... }: rec {
  # getShells :: list
  #   get all the named shells under a directory
  getShells = builtins.attrNames (builtins.readDir ../shells);

  # mkShells :: attrs
  #   generates multiple develop shells
  mkShells = nixpkgs.lib.genAttrs getShells mkShell;

  # mkShell :: str -> attrs
  #   generates a develop shell
  mkShell = name: import ../shells/${name} { inherit nixpkgs; };
}
