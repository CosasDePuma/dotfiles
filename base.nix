{ config, pkgs, ... }: {
  imports = [
    ./modules/documentation.nix
    ./modules/grub.nix
    ./modules/hardware.nix
    ./modules/network.nix
    ./modules/security.nix
    ./modules/software.nix
    ./modules/spanish.nix
    ./modules/users.nix
    ./modules/virtualisation.nix
  
    ./hardware-configuration.nix
  ];

  # Flakes
  nix.package = pkgs.nixUnstable;
  nix.extraOptions = "experimental-features = nix-command flakes";
  # Version
  system.stateVersion = "21.05";
}

