{ config, pkgs, ... }: {
  imports = [ ./hardware-configuration.nix ];

  # ·--------------·
  # |     Boot     |
  # ·--------------·
  # Whether to delete all files in /tmp during boot.
  boot.cleanTmpDir = true;
  # Whether the installation process is allowed to modify EFI boot variables.
  boot.loader.efi.canTouchEfiVariables = true;
  # Where the EFI System Partition is mounted.
  boot.loader.efi.efiSysMountPoint = "/boot";
  # Whether to enable the GNU GRUB boot loader.
  boot.loader.grub.enable = true;
  # Background color to be used for GRUB to fill the areas the image isn't filling.
  boot.loader.grub.backgroundColor = "#282A36";
  # GRUB entry name instead of default.
  boot.loader.grub.configurationName = "Malware";
  # Whether GRUB should be built with EFI support. EFI support is only available for GRUB v2. This option is ignored for GRUB v1.
  boot.loader.grub.efiSupport = true;
  # Background image used for GRUB. Set to null to run GRUB in text mode.
  boot.loader.grub.splashImage = null;
  # The version of GRUB to use = 1 for GRUB Legacy (versions 0.9x), or 2 (the default) for GRUB 2.
  boot.loader.grub.version = 2;

  # ·--------------·
  # |   Console    |
  # ·--------------·
  # The 16 colors palette used by the virtual consoles. Leave empty to use the default colors. Colors must be in hexadecimal format and listed in order from color 0 to color 15.
  console.colors = [
    "000000" "ff5555" "50fa7b" "f1fa8c" "bd93f9" "ff79c6" "8Be9fd" "bfbfbf"
    "4d4d4d" "ff6e67" "5af78e" "f4f99d" "caa9fa" "ff92d0" "9aedfe" "e6e6e6"
  ];
  # The keyboard mapping table for the virtual consoles.
  console.keyMap = "us";

  # ·--------------·
  # |  Containers  |
  # ·--------------·
  # A set of NixOS system configurations to be run as lightweight containers. Each container appears as a service container-name on the host system, allowing it to be started and stopped via systemctl.
  containers = { };

  # ·--------------·
  # |     Docs     |
  # ·--------------·
  # Whether to install documentation of packages from environment.systemPackages into the generated system path.
  documentation.enable = true;
  # Whether to install documentation targeted at developers.
  documentation.dev.enable = false;
  # Whether to install documentation distributed in packages' /share/doc. Usually plain text and/or HTML. This also includes "doc" outputs.
  documentation.doc.enable = false;
  # Whether to install info pages and the info command. This also includes "info" outputs.
  documentation.info.enable = false;
  # Whether to install manual pages and the man command. This also includes "man" outputs.
  documentation.man.enable = true;
  # Whether to generate the manual page index caches using mandb(8). This allows searching for a page or keyword using utilities like apropos(1).
  documentation.man.generateCaches = false;
  # Whether to install NixOS's own documentation.
  documentation.nixos.enable = false;

  # ·--------------·
  # | Environment  |
  # ·--------------·
  # Set of default packages that aren't strictly neccessary for a running system, entries can be removed for a more minimal NixOS installation.
  environment.defaultPackages =  with pkgs; [
    cool-retro-term # Terminal (alt.)
    emacs           # GUI Editor
  ];
  # An attribute set that maps aliases (the top level attribute names in this option) to command strings or directly to build outputs. The aliases are added to all users' shells. Aliases mapped to null are ignored.
  environment.shellAliases = {
    # cat
    cat =   "bat -p -P";
    catn =  "bat -P";
    less =  "bat --paging always";
    # ls
    l =     null;
    ls =    "lsd";
    ll =    "lsd -l";
    lla =   "lsd -la";
  };
  # The set of packages that appear in /run/current-system/sw. These packages are automatically available to all users, and are automatically updated every time you rebuild the system configuration. (The latter is the main difference with installing them in the default profile, /nix/var/nix/profiles/default.
  environment.systemPackages = with pkgs; [
    go              # Programming Language
    bat             # cat(1)
    chromium        # Browser
    curl            # Fetcher
    feh             # Background
    #fish            # Shell
    git             # Subversion
    kitty           # Terminal
    lsd             # ls(1)
    #neovim          # Terminal Editor
    pcmanfm         # Folder manager
    picom           # Compositor
    rofi            # Launcher
    wget            # Downloader
  ];

  # ·--------------·
  # |   Hardware   |
  # ·--------------·
  # Enable acpilight. This will allow brightness control via xbacklight from users in the video group
  hardware.acpilight.enable = true;
  # Whether to enable support for Bluetooth.
  hardware.bluetooth.enable = true;
  # Whether to enable OpenGL drivers. This is needed to enable OpenGL support in X11 systems, as well as for Wayland compositors like sway and Weston. It is enabled by default by the corresponding modules, so you do not usually have to set it yourself, only if there is no module for your wayland compositor of choice. See services.xserver.enable and programs.sway.enable.
  hardware.opengl.enable = true;
  # On 64-bit systems, whether to support Direct Rendering for 32-bit applications (such as Wine). This is currently only supported for the nvidia as well as Mesa.
  hardware.opengl.driSupport32Bit = true;
  # Whether to enable the PulseAudio sound server.
  hardware.pulseaudio.enable = true;
  # Enable udev rules for Steam hardware such as the Steam Controller, other supported controllers and the HTC Vive
  hardware.steam-hardware.enable = true;

  # ·--------------·
  # |   Language   |
  # ·--------------·
  # The default locale. It determines the language for program messages, the format for dates and times, sort order, and so on. It also determines the character set, such as UTF-8.
  i18n.defaultLocale = "es_ES.UTF-8";

  # ·--------------·
  # |  Networking  |
  # ·--------------·
  # Whether to enable support for IPv6.
  networking.enableIPv6 = true;
  # Additional verbatim entries to be appended to /etc/hosts. For adding hosts from derivation results, use networking.hostFiles instead.
  networking.extraHosts = "";
  # Whether to enable the firewall. This is a simple stateful firewall that blocks connection attempts to unauthorised TCP or UDP ports on this machine. It does not affect packet forwarding.
  networking.firewall.enable = true;
  # Whether to respond to incoming ICMPv4 echo requests ("pings"). ICMPv6 pings are always allowed because the larger address space of IPv6 makes network scanning much less effective.
  networking.firewall.allowPing = false;
  # Additional shell commands executed as part of the firewall initialisation script. These are executed just before the final "reject" firewall rule is added, so they can be used to allow packets that would otherwise be refused.
  networking.firewall.extraCommands = "";
  # The name of the machine. Leave it empty if you want to obtain it from a DHCP server (if using DHCP). The hostname must be a valid DNS label (see RFC 1035 section 2.3.1 = "Preferred name syntax", RFC 1123 section 2.1 = "Host Names and Numbers") and as such must not contain the domain part. This means that the hostname must start with a letter or digit, end with a letter or digit, and have as interior characters only letters, digits, and hyphen. The maximum length is 63 characters. Additionally it is recommended to only use lower-case characters. If (e.g. for legacy reasons) a FQDN is required as the Linux kernel network node hostname (uname --nodename) the option boot.kernel.sysctl."kernel.hostname" can be used as a workaround (but the 64 character limit still applies).
  networking.hostName = "malware";
  # The list of nameservers. It can be left empty if it is auto-detected through DHCP.
  networking.nameservers = [ "1.1.1.1" "1.0.0.1" ];
  # Whether to use NetworkManager to obtain an IP address and other configuration for all network interfaces that are not manually configured. If enabled, a group networkmanager will be created. Add all users that should have permission to change network settings to this group.
  networking.networkmanager.enable = true;
  # Set the MAC address of the interface.
  networking.networkmanager.ethernet.macAddress = "random";
  # List of interfaces that will not be managed by NetworkManager. Interface name can be specified here, but if you need more fidelity, refer to https =//developer.gnome.org/NetworkManager/stable/NetworkManager.conf.html#device-spec or the "Device List Format" Appendix of NetworkManager.conf(5).
  networking.networkmanager.unmanaged = [ "*" "except:type:wwan" "except:type:gsm" ];
  # Set the MAC address of the interface.
  networking.networkmanager.wifi.macAddress = "random";
  # Whether to use DHCP to obtain an IP address and other configuration for all network interfaces that are not manually configured.
  networking.useDHCP = true;
  # Whether to assign predictable names to network interfaces. If enabled, interfaces are assigned names that contain topology information (e.g. wlp3s0) and thus should be stable across reboots. If disabled, names depend on the order in which interfaces are discovered by the kernel, which may change randomly across reboots; for instance, you may find eth0 and eth1 flipping unpredictably.
  networking.usePredictableInterfaceNames = false;
  # Whether to enable wpa_supplicant.
  networking.wireless.enable = true;

  # ·--------------·
  # |     Nix      |
  # ·--------------·
  # A list of names of users (separated by whitespace) that are allowed to connect to the Nix daemon. As with nix.trustedUsers, you can specify groups by prefixing them with @. Also, you can allow all users by specifying *. The default is *. Note that trusted users are always allowed to connect.
  nix.allowedUsers = [ "@wheel" ];
  # If set to true, Nix automatically detects files in the store that have identical contents, and replaces them with hard links to a single copy. This saves disk space. If set to false (the default), you can still run nix-store --optimise to get rid of duplicate files.
  nix.autoOptimiseStore = true;
  # Automatically run the garbage collector at a specific time.
  nix.gc.automatic = true;
  # How often or when garbage collection is performed. For most desktop and server systems a sufficient garbage collection is once a week.
  nix.gc.dates = "weekly";
  # Automatically run the nix store optimiser at a specific time.
  nix.optimise.automatic = true;
  # Specification (in the format described by systemd.time(7)) of the time at which the optimiser will run.
  nix.optimise.dates = [ "weekly" ];
  # If set, NixOS will enforce the immutability of the Nix store by making /nix/store a read-only bind mount. Nix will automatically make the store writable when needed.
  nix.readOnlyStore = false;
  # If set, Nix will perform builds in a sandboxed environment that it will set up automatically for each build. This prevents impurities in builds by disallowing access to dependencies outside of the Nix store by using network and mount namespaces in a chroot environment. This is enabled by default even though it has a possible performance impact due to the initial setup time of a sandbox for each build. It doesn't affect derivation hashes, so changing this option will not trigger a rebuild of packages.
  nix.useSandbox = true;
  # The configuration of the Nix Packages collection. (For details, see the Nixpkgs documentation.) It allows you to set package configuration options.
  nixpkgs.config = { allowUnfree = true; };

  # ·--------------·
  # |   Programs   |
  # ·--------------·
  # Whether to configure system to use Android Debug Bridge (adb). To grant access to a user, it must be part of adbusers group: users.users.alice.extraGroups = ["adbusers"];
  programs.adb.enable = true;
  # Whether to enable chromium policies.
  programs.chromium.enable = true;
  # Chromium default search provider url.
  programs.chromium.defaultSearchProviderSearchURL = "https://duckduckgo.com/?q={searchTerms}";
  # List of chromium extensions to install. For list of plugins ids see id in url of extensions on chrome web store page. To install a chromium extension not included in the chrome web store, append to the extension id a semicolon ";" followed by a URL pointing to an Update Manifest XML file. See ExtensionInstallForcelist for additional details.
  programs.chromium.extensions = [
    "gighmmpiobklfepjocnamgkkbiglidom"  # AdBlock
    "gcknhkkoolaabfmlnjonogaaifnjlfnp"  # FoxyProxy Standard
    "bkkbcggnhapdmkeljlodobbkopceiche"  # PopUp Blocker
    "gppongmhjkpfnbhagpmjfkannfbllamg"  # Wappalyzer
  ];
  # Chromium default homepage
  programs.chromium.homepageLocation = "https://duckduckgo.com/";
  # Whether to configure fish as an interactive shell.
  programs.fish.enable = true;
  # Install and setup the Java development kit.
  programs.java.enable = true;
  # Java package to install. Typical values are pkgs.jdk or pkgs.jre.
  programs.java.package = "pkgs.jdk";
  # Whether to enable Neovim.
  programs.neovim.enable = true;
  # When enabled, installs neovim and configures neovim to be the default editor using the EDITOR environment variable.
  programs.neovim.defaultEditor = true;
  # Symlink vi to nvim binary.
  programs.neovim.viAlias = true;
  # Symlink vim to nvim binary.
  programs.neovim.vimAlias = true;
  # Whether to enable nm-applet.
  programs.nm-applet.enable = true;
  # Whether to enable npm global config.
  programs.npm.enable = true;
  # Whether to enable installing proxychains configuration.
  programs.proxychains.enable = true;
  # The package used for the openssh client and daemon.
  programs.ssh.package = "pkgs.openssh";
  # Whether to enable steam.
  programs.steam.enable = true;
  # Open ports in the firewall for Source Dedicated Server.
  programs.steam.dedicatedServer.openFirewall = true;
  # Open ports in the firewall for Steam Remote Play.
  programs.steam.remotePlay.openFirewall = true;
  # Whether to enable thefuck.
  programs.thefuck.enable = true;
  # `thefuck` needs an alias to be configured. The default value is `fuck`, but you can use anything else as well.
  programs.thefuck.alias  = "fuck";
  # Whether to add Wireshark to the global environment and configure a setcap wrapper for 'dumpcap' for users in the 'wireshark' group.
  programs.wireshark.enable = true;
  # Which Wireshark package to install in the global environment.
  programs.wireshark.package  = "pkgs.wireshark";
  # Whether to enable ZMap.
  programs.zmap.enable = true;

  # ·--------------·
  # |   Security   |
  # ·--------------·
  # Accept the CA's terms of service. The default provider is Let's Encrypt, you can find their ToS at https://letsencrypt.org/repository/.
  security.acme.acceptTerms = true;
  # Attribute set of certificates to get signed and renewed. Creates acme-${cert}.{service,timer} systemd units for each certificate defined here. Other services can add dependencies to those units if they rely on the certificates being present, or trigger restarts of the service if certificates get renewed.
  ## Contact email address for the CA to be able to reach you.
  security.acme.certs."kike.wtf".email = "hola+acme@kike.wtf";
  ## A list of extra domain names, which are included in the one certificate to be issued.
  security.acme.certs."kike.wtf".extraDomainNames = [ "*.kike.wtf" "www.kike.wtf" ];
  ## Group running the ACME client.
  security.acme.certs."kike.wtf".group = "acme";
  ## Where the webroot of the HTTP vhost is located. .well-known/acme-challenge/ directory will be created below the webroot if it doesn't exist. http://example.org/.well-known/acme-challenge/ must also be available (notice unencrypted HTTP).
  security.acme.certs."kike.wtf".webroot = "/www/kike.wtf";
  # Whether to enable the sudo command, which allows non-root users to execute commands as root.
  security.sudo.enable = true;
  # Only allow members of the wheel group to execute sudo by setting the executable's permissions accordingly. This prevents users that are not members of wheel from exploiting vulnerabilities in sudo such as CVE-2021-3156.
  security.sudo.execWheelOnly = true;
  # Whether users of the wheel group must provide a password to run commands as super user via sudo.
  security.sudo.wheelNeedsPassword = true;

  # ·--------------·
  # |   Services   |
  # ·--------------·
  # Whether to enable the ACPI daemon.
  services.acpid.enable = true;
  # Whether to enable handling of hotplug and sleep events by autorandr.
  services.autorandr.enable = true;
  # Whether to enable the OpenSSH secure shell daemon, which allows secure remote logins.
  services.openssh.enable = true;
  # Whether to allow X11 connections to be forwarded.
  services.openssh.forwardX11 = true;
  # Whether to automatically open the specified ports in the firewall.
  services.openssh.openFirewall = true;
  # Whether the root user can login using ssh.
  services.openssh.permitRootLogin = "no";
  # If set, sshd is socket-activated; that is, instead of having it permanently running as a daemon, systemd will start an instance for each incoming connection.
  services.openssh.startWhenNeeded = true;
  # Whether to enable MySQL server.
  services.mysql.enable = true;
  # Which MySQL derivation to use. MariaDB packages are supported too.
  services.mysql.package = "pkgs.mariadb";
  # Whether to enable the Apache HTTP Server.
  services.httpd.enable = true;
  # Whether to enable the PHP module.
  services.httpd.enablePHP = true;
  # Options appended to the PHP configuration file php.ini.
  services.httpd.phpOptions = "";
  # Whether to ask Let's Encrypt to sign a certificate for this vhost. Alternately, you can use an existing certificate through useACMEHost.
  services.httpd.virtualHosts."kike.wtf".enableACME = true;
  # Whether to enable HTTPS in addition to plain HTTP. This will set defaults for listen to listen on all interfaces on the respective default ports (80, 443).
  services.httpd.virtualHosts."kike.wtf".addSSL = true;
  # E-mail address of the server administrator.
  services.httpd.virtualHosts."kike.wtf".adminAddr = "hola+admin@kike.wtf";
  # The path of Apache's document root directory. If left undefined, an empty directory in the Nix store will be used as root.
  services.httpd.virtualHosts."kike.wtf".documentRoot = "/www/kike.wtf";
  # Whether to add a separate nginx server block that permanently redirects (301) all plain HTTP traffic to HTTPS. This will set defaults for listen to listen on all interfaces on the respective default ports (80, 443), where the non-SSL listens are used for the redirect vhosts.
  services.httpd.virtualHosts."kike.wtf".forceSSL = true;
  # Declarative location config. See https://httpd.apache.org/docs/2.4/mod/core.html#location for details.
  services.httpd.virtualHosts."kike.wtf".locations = {
    "~ \.php$" = {
      extraConfig = ''
        fastcgi_pass unix:${config.services.phpfpm.pools.mypool.socket};
        fastcgi_index index.php;
      '';
    };
    "/.well-known/acme-challenge" = {
      root = "/var/lib/acme/challenges";
    };
  };
  # Specification of pages to be ignored by web crawlers. See http://www.robotstxt.org/ for details.
  services.httpd.virtualHosts."kike.wtf".robotsEntries = "User-agent: *\nDisallow: /";
  # Additional names of virtual hosts served by this virtual host configuration.
  services.httpd.virtualHosts."kike.wtf".serverAliases = [ "www.kike.wtf" ];
  # Whether to enable the X server.
  services.xserver.enable = true;
  # When set to true the wallpaper will stretch across all screens. When set to false the wallpaper is duplicated to all screens.
  services.xserver.desktopManager.wallpaper.combineScreens = false;
  # The file ~/.background-image is used as a background image. This option specifies the placement of this image onto your desktop.
  services.xserver.desktopManager.wallpaper.mode = "fill";
  # Automatically log in as autoLogin.user.
  services.xserver.displayManager.autoLogin.enable = true;
  # User to be used for the automatic login.
  services.xserver.displayManager.autoLogin.user = "puma";
  # Graphical session to pre-select in the session chooser (only effective for GDM and LightDM).
  services.xserver.displayManager.defaultSession = "none+i3";
  # Whether to enable lightdm as the display manager.
  services.xserver.displayManager.lightdm.enable = true;
  # The background image or color to use.
  services.xserver.displayManager.lightdm.background = "pkgs.nixos-artwork.wallpapers.simple-dark-gray-bottom.gnomeFilePath";
  # Keyboard layout, or multiple keyboard layouts separated by commas.
  services.xserver.layout = "us,es";
  # Whether to enable libinput.
  services.xserver.libinput.enable = true;
  # Whether to use the Glamor module for 2D acceleration, if possible.
  services.xserver.useGlamor = true;
  # Whether to enable i3 window manager.
  services.xserver.windowManager.i3.enable = true;
  # i3 package to use.
  services.xserver.windowManager.i3.package = "pkgs.i3-gaps";
  # Extra packages to be installed system wide.
  services.xserver.windowManager.i3.extraPackages = [ pkgs.i3blocks-gaps ];
  # X keyboard options; layout switching goes here.
  services.xserver.xkbOptions = "grp:alt_shift";

  # ·--------------·
  # |    Sound     |
  # ·--------------·
  # Whether to enable ALSA sound.
  sound.enable = true;

  # ·--------------·
  # |     Swap     |
  # ·--------------·
  # The swap devices and swap files. These must have been initialised using mkswap. Each element should be an attribute set specifying either the path of the swap device or file (device) or the label of the swap device (label, see mkswap -L). Using a label is recommended.
  swapDevices = [ { label = "swap" } ];

  # ·--------------·
  # |    System    |
  # ·--------------·
  # The NixOS release (e.g. 16.03).
  system.nixos.release = "21.05";
  # Every once in a while, a new NixOS release may change configuration defaults in a way incompatible with stateful data. For instance, if the default version of PostgreSQL changes, the new version will probably be unable to read your existing databases. To prevent such breakage, you should set the value of this option to the NixOS release with which you want to be compatible. The effect is that NixOS will use defaults corresponding to the specified release (such as using an older version of PostgreSQL). It‘s perfectly fine and recommended to leave this value at the release version of the first install of this system. Changing this option will not upgrade your system. In fact it is meant to stay constant exactly when you upgrade your system. You should only bump this option, if you are sure that you can or have migrated all state on your system which is affected by this option.
  system.stateVersion = "21.05";

  # ·--------------·
  # |     User     |
  # ·--------------·
  # This option defines the default shell assigned to user accounts. This can be either a full system path or a shell package.
  users.defaultUserShell = "pkgs.fish";
  # If set to true, you are free to add new users and groups to the system with the ordinary useradd and groupadd commands. On system activation, the existing contents of the /etc/passwd and /etc/group files will be merged with the contents generated from the users.users and users.groups options. The initial password for a user will be set according to users.users, but existing passwords will not be changed.
  users.mutableUsers = true;
  # Additional user accounts to be created automatically by the system. This can also be used to set options for root.
  ## A short description of the user account, typically the user's full name. This is actually the “GECOS” or “comment” field in /etc/passwd.
  users.users."puma".description = "Puma at the zoo";
  ## The user's auxiliary groups.
  users.users."puma".extraGroups = [
    "adbusers"        # Android
    "acme"            # SSL
    "docker"          # Docker
    "networkmanager"  # Network
    "wheel"           # Sudo
    "wireshark"       # Wireshark
    "vboxusers"       # VirtualBox
    "video"           # Brightness
  ];
  ## Specifies the initial password for the user, i.e. the password assigned if the user does not already exist. If users.mutableUsers is true, the password can be changed subsequently using the passwd command. Otherwise, it's equivalent to setting the password option. The same caveat applies: the password specified here is world-readable in the Nix store, so it should only be used for guest accounts or passwords that will be changed promptly.
  users.users."puma".initialPassword = "CHANGEME!";
  ## Indicates whether this is an account for a “real” user. This automatically sets group to users, createHome to true, home to /home/username, useDefaultShell to true, and isSystemUser to false. Exactly one of isNormalUser and isSystemUser must be true.
  users.users."puma".isNormalUser = true;

  # ·--------------·
  # |Virtualisation|
  # ·--------------·
  # This option enables docker, a daemon that manages linux containers. Users in the "docker" group can interact with the daemon (e.g. to start or stop containers) using the docker command line tool.
  virtualisation.docker.enable = true;
  # When enabled dockerd is started on boot. This is required for containers which are created with the --restart=always flag to work. If this option is disabled, docker might be started on demand by socket activation.
  virtualisation.docker.enableOnBoot = true;
  # Whether to periodically prune Docker resources. If enabled, a systemd timer will run docker system prune -f as specified by the dates option.
  virtualisation.docker.autoPrune.enable = true;
  # Whether to enable VirtualBox.
  virtualisation.virtualbox.host.enable = true;
  # Whether to install the Oracle Extension Pack for VirtualBox.
  virtualisation.virtualbox.host.enableExtensionPack = true;
  # Enable hardened VirtualBox, which ensures that only the binaries in the system path get access to the devices exposed by the kernel modules instead of all users in the vboxusers group.
  virtualisation.virtualbox.host.enableHardening = true;
}
