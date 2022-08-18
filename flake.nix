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
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      # --- imports --- #

      lib = import ./lib { inherit nixpkgs system; };
      overlays = import ./overlays { inherit pkgs; };
    in {
      devShells."${system}" = lib.mkShells;

      nixosConfigurations = lib.mkMachines {
        boring = {
          inherit overlays;
          hardware = "matebook";
        };

        mobile = {
          inherit overlays;
          hardware = "vmware";
        };
      };
    };
}
