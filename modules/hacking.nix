{ config, lib, pkgs, ... }:
with lib; {
  config = {
  programs.wireshark.enable = mkDefault true;
  environment.systemPackages = with pkgs; mkDefault [
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
      zap          # web security platform (open-source)
    ];
  };
}