{ config, ... }:
let
  cfg = config;
in {
  config = {
    security = {
      # Prevent replacing the running kernel w/o reboot
      protectKernelImage = true; 
      # ACME SSL
      acme.acceptTerms = true;
    };

    boot = {
      # Clear tmp files
      tmpOnTmpfs = true;
      cleanTmpDir = !cfg.boot.tmpOnTmpfs;
      # Disable systemd-boot editor
      loader.systemd-boot.editor = false;
      # Remove Magic SysRQ key
      kernel.sysctl."kernel.sysrq" = 0;
      # Prevent ICMP errors filling up the logs
      kernel.sysctl."net.ipv4.icmp_ignore_bogus_error_responses" = 1;
      # Mitigate IP Spoofing
      kernel.sysctl."net.ipv4.conf.all.rp_filter" = 1;
      kernel.sysctl."net.ipv4.conf.default.rp_filter" = 1;
      # Mitigate MitM
      kernel.sysctl."net.ipv4.conf.all.accept_redirects" = 0;
      kernel.sysctl."net.ipv4.conf.default.accept_redirects" = 0;
      kernel.sysctl."net.ipv4.conf.all.secure_redirects" = 0;
      kernel.sysctl."net.ipv4.conf.default.secure_redirects" = 0;
      kernel.sysctl."net.ipv6.conf.all.accept_redirects" = 0;
      kernel.sysctl."net.ipv6.conf.default.accept_redirects" = 0;
      # Mitigate TIME-WAIT assasination
      kernel.sysctl."net.ipv4.tcp_rfc1337" = 1;
      # Protect against SYN flood attacks
      kernel.sysctl."net.ipv4.tcp_syncookies" = 1;
      # Disable routed packages (we aren't a router)
      kernel.sysctl."net.ipv4.conf.all.send_redirects" = 0;
      kernel.sysctl."net.ipv4.conf.default.send_redirects" = 0;
      kernel.sysctl."net.ipv4.conf.all.accept_source_route" = 0;
      kernel.sysctl."net.ipv6.conf.all.accept_source_route" = 0;
      # TCP optimization
      kernel.sysctl."net.ipv4.tcp_fastopen" = 3;
      kernel.sysctl."net.core.default_qdisc" = "cake";
      kernel.sysctl."net.ipv4.tcp_congestion_control" = "bbr";
      kernelModules = [ "tcp_bbr" ];
    };
    
    nix = {
      # Nix optimization
      autoOptimiseStore = true;
      optimise.automatic = true;
      optimise.dates = [ "daily" ];
      # Nix garbage collector
      gc.automatic = true;
      gc.dates = "daily";
    };
  };
}