{
  # -- 🐯 Pumita OS -- #

  description = "Pumita's flake";

  # -- 📥 inputs -- #
  
  inputs = {
    # nixos packages
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    
    # home manager
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  # -- 📦 outputs -- #

  outputs = inputs @ { self, nixpkgs, home-manager, ... }:
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
              owner = "jonaburg";
              rev = "v8";
              sha256 = "sha256-JYVk7mq9K9wm/WLDy/q7ghrVcigkGUWT9QiPs89eWxM=";
            };
          });
        })
      ];

      # -- 🧠 functions -- #

      # generates the home manager config for a user
      mkHome = { user, machine, ... }: {
        "${user}" = {
          imports = [
            # user configuration
            ({
              programs.home-manager.enable = true;
              home = {
                username = user;
                stateVersion = nixosVersion;
                homeDirectory = if user == "root" then "/root" else "/home/${user}";
              };
            })
            # custom configuration for the user
            (./machines/${machine}/home.nix)
          ];
        };
      };

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
            # home manager
            home-manager.nixosModules.home-manager {
              home-manager = {
                # home manager configuration
                useGlobalPkgs = true;
                useUserPackages = true;
                # users configuration
                users = mkHome { inherit user; inherit machine; }; #) ++ (mkHome "root");
              };
            }
            # hostname
            ({ networking.hostName = machine; })
            # user configutation
            ({
              users.users."${user}" = {
                isNormalUser = true;
                extraGroups = [ "networkmanager wheel" ] ++ extraGroups;
              };
            })
            # machine configuration
            (./machines/${machine})
            # hardware configuration
            (./hardware/${hardware})
          
          # extra modules
          ] ++ extraModules;
        };
    in {
      nixosConfigurations = {

        bounty = mkMachine {
          inherit overlays; 
          machine = "bounty";
          user = "bug";
          hardware = "vmware";
        };

      };
    };
}
