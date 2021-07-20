{ config, pkgs, ... }: {
  imports = [ ./hardware-configuration.nix ];

  # ·--------------·
  # |     Boot     |
  # ·--------------·

  boot.kernelModules = [ ];
  boot.loader = {
    # UEFI
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  # ·--------------·
  # |     User     |
  # ·--------------·

  users.users = {

    # Main user
    dexter = {
      isNormalUser = true;
      home = "/home/dexter";
      description = "Dexter's Laboratory";
      extraGroups = [
        "networkmanager"	# Network
        "video"			# Video
        "wheel"			# Sudo
      ];
    };

    # Services
    #nginx.extraGroups = [ "acme" ];

  };

  # ·--------------·
  # |   Packages   |
  # ·--------------·

  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
  
    bat				# cat(1)
    cool-retro-term		# Terminal (alt.)
    chromium			# Browser
    curl			# Fetcher
    emacs			# GUI Editor
    feh				# Background
    fish			# Shell
    git				# Subversion
    go				# Programming Language
    kitty			# Terminal
    lsd				# ls(1)
    neovim			# Terminal Editor
    pcmanfm			# Folder manager
    picom			# Compositor
    rofi			# Launcher
    wget			# Downloader

  ];

  # ·--------------·
  # |   Services   |
  # ·--------------·
  services = {
  
    # Printer
    printing.enable = false;

    # SSH
    openssh = {
      enable = false;
      permitRootLogin = "no";
    };

    # SQL
    mysql = {
      enable = false;
      package = pkgs.mariadb;
    };

    # HTTP
    nginx = {
      enable = false;
      virtualHosts = {
        "kike.wtf" = {
          serverAliases = [ "www.kike.wtf" ];
          addSSL = true;
          forceSSL = true;
          enableACME = true;
          locations = {
            "~ \.php$" = {
              extraConfig = ''
                fastcgi_pass unix:${config.services.phpfpm.pools.mypool.socket};
                fastcgi_index index.php;
              '';
            };
            "/.well-known/acme-challenge" = {
              root = "/var/lib/acme/.challenges";
            };
          };
        };
      };
    };

    # PHP
    phpfpm.pools.mypool = {
      user = "nobody";
      settings = {
        pm = "dynamic";
        "listen.owner" = config.services.nginx.user;
        "pm.max_children" = 5;
        "pm.start_servers" = 2;
        "pm.min_spare_servers" = 1;
        "pm.max_spare_servers" = 3;
        "pm.max_requests" = 500;
      };
    };
  
    # X Window System
    xserver = {
      enable = true;
      autorun = true;

      # Keyboard
      layout = "us,es";
      xkbOptions = "grp:menu_toggle";

      # Touchpad
      libinput.enable = true;

      # Display Manager
      displayManager = {
        defaultSession = "none+i3";
        autoLogin = {
          enable = true;
          user = "dexter";
        };
      };

      # Window Manager
      windowManager.i3 = {
        enable = true;
	package = pkgs.i3-gaps;
      };

      # Drivers
      videoDrivers = [ "modesetting" "nvidia" ];
      useGlamor = true;
    };
 
  };

  # ·--------------·
  # |   Security   |
  # ·--------------·

  security = {

    # Let's Encrypt (TLS)
    acme = {
      email = "hola@kike.wtf";
      acceptTerms = true;
      certs = {
        "kike.wtf" = {
          webroot = "/www/kike.wtf";
          group = "nginx";
          extraDomainNames = [ "*.kike.wtf" ];
        };
      };
    };

  };

  # ·--------------·
  # |     Disk     |
  # ·--------------·

  fileSystems = {
    
    # Secondary disk
    "/ext" = {
      device = "/dev/disk/by-label/ext";
      fsType = "ext4";
      options = [ "nofail" ];
    };
  
  };

  # ·--------------·
  # |  Networking  |
  # ·--------------·

  networking = {
    
    # Host
    hostName = "lab";
    extraHosts = ''
      127.0.0.2 dexter.lab
    '';
  
    # Manager
    networkmanager = {
      enable = true;
      unmanaged = [
        "*" "except:type:wwan" "except:type:gsm"
      ];
    };

    # WiFi
    wireless = {
      enable = true;
      interfaces = [ "wlan0" "wlan1" ];
    };

    # Interfaces
    enableIPv6 = true;
    usePredictableInterfaceNames = true;
    nameservers = [ "1.1.1.1" "1.0.0.1" ];

    # Firewall
    firewall = {
      enable = true;
      allowPing = false;
      allowedTCPPorts = [ 22 80 443 ];
      allowedUDPPorts = [ ];
    };
  
  };

  # ·--------------·
  # |   Hardware   |
  # ·--------------·

  hardware = {
    
    enableAllFirmware = false;
    
    # Audio
    pulseaudio.enable = true;
    
    # OpenGL
    opengl = {
      driSupport = true;
      driSupport32Bit = true;
      extraPackages = [
        pkgs.intel-compute-runtime
      ];
    };
  
  };

  # ·--------------·
  # |    System    |
  # ·--------------·

  nix = {
    # Garbage Collector
    gc = {
      automatic = true;
      dates = "18.00";
    };
  };

  # Time
  time.timeZone = "Europe/Madrid";

  # Internationalisation
  i18n.defaultLocale = "es_ES.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Sound
  sound.enable = true;

  system.stateVersion = "21.05";
}
