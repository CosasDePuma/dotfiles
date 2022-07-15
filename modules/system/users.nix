{ config, lib, hostname ? "nixos", ... }:
let
  cfg = config.system.users;
  mkUser = { user, description ? "", sudo ? false, groups ? [], ... }: {
    inherit description;
    extraGroups = (lib.optional sudo "wheel") ++ groups;
    isNormalUser = true;
  };
in {
  options = {
    system.users = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      example = { "puma" = { sudo = true; groups = [ "networkmanager" ]; }; };
      description = "Regular users of the system.";
    };
  };

  config = {
    users.users = builtins.mapAttrs (user: v: mkUser ({ inherit user; } // v)) cfg;
  };
}
