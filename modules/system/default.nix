{ lib, ... }: {
  imports = builtins.map (x: ./${x})
    (builtins.attrNames
      (lib.filterAttrs (n: v: n != "default.nix" && v == "regular")
        (builtins.readDir ./.)));
}
