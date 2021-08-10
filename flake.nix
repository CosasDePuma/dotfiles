{
  description = "My machines!";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.05";
    home-manager.url = "github:nix-community/home-manager/release-21.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, ... }:
  let
    # Only install Linux systems
    system = "x86_64-linux";
    # Available programs
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };

    # Default modules needed in every system
    defaultModules = map (n: "common/${n}") [
      "documentation"
      "grub"
      "hardware"
      "network"
      "security"
      "software"
      "spanish"
      "virtualisation"
   ];

    # Get all hostnames from hosts folder
    getHosts = builtins.attrNames (nixpkgs.lib.attrsets.filterAttrs (_: v: v == "directory") (builtins.readDir ./hosts));

    # Converts modules filenames to paths
    # Example: mkModules [ "hardware" "spanish" ]
    mkModules = modules: map (n: ./modules + "/${n}.nix" ) modules;
    
    # Converts hosts to VM hosts
    # Example: mkVMHosts [ "hardware" "spanish" ]
    vmSuffix = "vm";
    mkVMHosts = hosts: map (n: "${n}${vmSuffix}" ) hosts;

    # Creates the configuration for each host from a list of hostnames
    # Example: mkHosts [ "personal" "work" ]
    mkHosts = hosts: nixpkgs.lib.attrsets.genAttrs hosts (host: nixpkgs.lib.nixosSystem {
      inherit system;
      modules =
        with nixpkgs.lib;
        let
          hostname = strings.removeSuffix vmSuffix host;
        in (mkModules defaultModules) ++ [
          # Version
          ({ system.stateVersion = "21.05"; })
          # Flakes
          ({ pkgs, ... }: {
            nix.package = pkgs.nixFlakes;
            nix.extraOptions = "experimental-features = nix-command flakes";
          })
          # Hostname
          ({ networking.hostName = hostname; })
          # VM settings
          ({ pkgs, ... }: mkIf (strings.hasSuffix vmSuffix host) {
            virtualisation.vmware.guest.enable = true;
            virtualisation.virtualbox.guest.enable = true;
            environment.systemPackages = with pkgs; [ "open-vm-tools" ];
          })
          # Custom configuration
          (./hosts + "/${hostname}")
          # Custom hardware configuration
          ./hardware-configuration.nix
      ];
    });
  in {
    # Systems
    nixosConfigurations = mkHosts (getHosts ++ (mkVMHosts getHosts));
  };
}
