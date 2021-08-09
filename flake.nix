{
  description = "My machines!";
  
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.05";
    home-manager.url = "github:nix-community/home-manager/release-21.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };

    mkHost = hostname: nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [
        ./base.nix
        (./hosts + "/${hostname}.nix")
      ];
    };
  in {
    nixosConfigurations.bounty = mkHost "bounty";
  };
}
