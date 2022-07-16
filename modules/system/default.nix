{ lib, ... }: {
  imports = builtins.map (x: ./${x})
    (builtins.attrNames
      (lib.filterAttrs (n: _: n != "default.nix")
        (builtins.readDir ./.)));
}
