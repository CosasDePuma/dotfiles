
{ nixpkgs, system, nixosVersion, ... }: rec {
  # mkMachines :: list -> attrs
  #   generates multiples nixosConfigutations
  mkMachines = machines: builtins.mapAttrs (hostname: v: mkMachine ({ inherit hostname; } // v)) machines;

  # mkMachine :: attrs -> attrs
  #   generates a nixosConfiguration based on the machine hostname
  mkMachine = { hostname, hardware ? "vmware", overlays ? [], extraModules ? [], ... }:
    nixpkgs.lib.nixosSystem {
      inherit system;
      # -- modules --
      modules = [
        # nixos version
        ({ system.stateVersion = nixosVersion; })
        # overlays
        ({ nixpkgs.overlays = overlays; })
        # hardware configuration
        (../hardware/${hardware}.nix)
        # machine configuration
        (../machines/${hostname}.nix)
        # extra modules
      ] ++ extraModules;
      # -- args --
      specialArgs = { inherit hostname; };
    };
  }
