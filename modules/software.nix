{ config, lib, pkgs, ... }:
with lib; {
  config = {
    # Allow unfree (Stallman crying...)
    nixpkgs.config.allowUnfree = true;
    # Common software
    environment.systemPackages = with pkgs; [ curl git unzip wget zip ];
    # Default shell
    programs.fish.enable = true;
    users.defaultUserShell = pkgs.fish;
    # Default editor
    programs.neovim.enable = true;
    programs.neovim.defaultEditor = true;
    programs.neovim.viAlias = true;
    programs.neovim.vimAlias = true;
    # Nice program
    programs.thefuck.enable = true;
    programs.thefuck.alias = "fuck";
  };
}