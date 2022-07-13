{
  # -- 🐯 Pumita OS -- #

  description = "Pumita's flake";

  # -- 📥 inputs -- #
  
  inputs = {
    # nixos packages
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  # -- 📦 outputs -- #

  outputs = inputs @ { self, nixpkgs, ... }:
    let
      # variables
      nixosVersion = "22.05";
      system = "x86_64-linux";
      # packages
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      # overlays
      overlays = [
        # picom overlay
        (self: super: {
          picom = pkgs.picom.overrideAttrs (_: {
            src = pkgs.fetchFromGitHub {
              repo = "picom";
              owner = "pijulius";
              rev = "e3c19cd7d1108d114552267f302548c113278d45";
              sha256 = "sha256-4voCAYd0fzJHQjJo4x3RoWz5l3JJbRvgIXn1Kg6nz6Y=";
            };
          });
        })
      ];

      # -- 🧠 functions -- #

      # gets the name of the machines from the machines directory
      getMachines = builtins.attrNames (nixpkgs.lib.attrsets.filterAttrs (_: v: v == "directory") (builtins.readDir ./machines));

      # generates the machine configuration
      mkMachine = { machine, user ? "puma", extraGroups ? [], extraModules ? [], overlays ? [], hardware ? "vmware", ... }:
        nixpkgs.lib.nixosSystem {
          inherit system;
          
          # modules
          modules = [
            # nixos version
            ({ system.stateVersion = nixosVersion; })
            # overlays
            ({ nixpkgs.overlays = overlays; })
            # flakes
            ({ pkgs, ... }: {
              nix = {
                package = pkgs.nixFlakes;
                extraOptions = "experimental-features = nix-command flakes";
              };
            })
            # hostname
            ({ networking.hostName = machine; })
            # user configutation
            ({
              users.users."${user}" = {
                isNormalUser = true;
                extraGroups = [ "networkmanager wheel" ] ++ extraGroups;
              };
            })
            # hardware configuration
            (./hardware/${hardware})
            # machine configuration
            (./machines/${machine}.nix)
          
          # extra modules
          ] ++ extraModules;
        };
    in {
      nixosConfigurations = {

        boring = mkMachine {
          inherit overlays;
          machine = "boring";
          user = "puma";
          hardware = "matebook.nix";
        };

        boringvm = mkMachine {
          inherit overlays; 
          machine = "boring";
          user = "puma";
          hardware = "vmware.nix";
        };

      };
    };
}
