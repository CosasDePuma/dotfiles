{ config, pkgs, ... }:
let
  user = "bug";
in {
  # User
  users.users."${user}" = {
    uid = 1000;
    createHome = true;
    isNormalUser = true;
    description = "Researcher";
    initialPassword = "CHANGEME";
    extraGroups = [ "network" "wheel" "wireshark" ];
  };
  services.xserver.displayManager.autoLogin.user = "${user}";

  home-manager.users."${user}" = { pkgs, ... }: {
    # Programs
    home.packages = with pkgs; [
      amass        # dns enumeration
      burpsuite    # web security platform
      ffuf         # bruteforce assets
      findomain    # subdomain discovery
      gau          # known urls
      gobuster     # bruteforce assets
      httpx        # http toolkit
      metasploit   # metasploit xdd
      naabu        # syn/connect port scanner
      nmap         # port scanner
      nuclei       # targeted scanning
      sqlmap       # automatic sql injection
      subfinder    # subdomain discovery
      wireshark    # network protocol analyzer
      wpscan       # wordpress
      zap          # web security platform (open-source)
    ];

    # Configuration
    xsession.windowManager.i3 = import ../../config/i3.nix pkgs; 
  };
}
