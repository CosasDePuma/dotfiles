{ config, pkgs, ... }:

{
  # External config files
  imports = [ ./hardware-configuration.nix ];
  # Boot loader
  boot.loader = {
    # UEFI
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };
  # Networking
  networking = {
    hostName = "nasa";				# Hostname
    wireless.enable = false;			# Enable WiFi
    networkmanager.enable = true;		# Enable NetworkManager
    useDHCP = false;				# Global DHCP (*Deprecated*)
    interfaces = {
      wlp0s20f3.useDHCP = true;			# DHCP (WiFi)
      enp0s20f0u1u4.useDHCP = true;		# DHCP (Ethernet)
    };
  };
  # Time
  time.timeZone = "Europe/Madrid";		# Timezone
  # Internationalisation
  i18n.defaultLocale = "es_ES.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  # X11 windowing system.
  services.xserver = {
    enable = true;
    displayManager.gdm.enable = true;		# GDM Display Manager
    desktopManager.gnome.enable = true;		# Gnome Window Manager
    # Keymap
    layout = "us,es";
    xkbOptions = "grp:menu_toggle";
    # Touchpad support
    libinput.enable = true;
  };
  # Service
  services = {
    printing.enable = true;			# Printer (CUPS)
    openssh.enable = true;			# SSH
  };
  # Sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  # User account
  users.users.c3b0 = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };
  # Packages
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    # Languages
    go
    # Desktop
    i3blocks-gaps
    i3-gaps
    # Editors
    emacs
    neovim
    # GUI
    chromium
    obs-studio
    pcmanfm
    spotify
    vscodium
    # Terminal
    cool-retro-term
    fish
    thefuck
    kitty
    # Utilities
    curl
    feh
    git
    picom
    rofi
    wget
  ];

  # Version control
  system.stateVersion = "21.05";
}

