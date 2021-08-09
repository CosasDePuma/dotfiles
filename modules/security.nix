{ config, lib, ... }:
with lib;
let
  cfg = config;
in {
  imports = [ ../services/rmlogs.nix ];

  config = {  
    # Prevent replacing the running kernel w/o reboot
    security.protectKernelImage = true; 
    # Clear tmp files
    boot.tmpOnTmpfs = mkDefault true;
    boot.cleanTmpDir = !cfg.boot.tmpOnTmpfs;
    # Disable systemd-boot editor
    boot.loader.systemd-boot.editor = false;
    # ACME SSL
    security.acme.acceptTerms = mkDefault true;
    # Remove Magic SysRQ key
    boot.kernel.sysctl."kernel.sysrq" = 0;
    # Prevent ICMP errors filling up the logs
    boot.kernel.sysctl."net.ipv4.icmp_ignore_bogus_error_responses" = 1;
    # Mitigate IP Spoofing
    boot.kernel.sysctl."net.ipv4.conf.all.rp_filter" = 1;
    boot.kernel.sysctl."net.ipv4.conf.default.rp_filter" = 1;
    # Mitigate MitM
    boot.kernel.sysctl."net.ipv4.conf.all.accept_redirects" = 0;
    boot.kernel.sysctl."net.ipv4.conf.default.accept_redirects" = 0;
    boot.kernel.sysctl."net.ipv4.conf.all.secure_redirects" = 0;
    boot.kernel.sysctl."net.ipv4.conf.default.secure_redirects" = 0;
    boot.kernel.sysctl."net.ipv6.conf.all.accept_redirects" = 0;
    boot.kernel.sysctl."net.ipv6.conf.default.accept_redirects" = 0;
    # Mitigate TIME-WAIT assasination
    boot.kernel.sysctl."net.ipv4.tcp_rfc1337" = 1;
    # Protect against SYN flood attacks
    boot.kernel.sysctl."net.ipv4.tcp_syncookies" = 1;
    # Disable routed packages (we aren't a router)
    boot.kernel.sysctl."net.ipv4.conf.all.send_redirects" = 0;
    boot.kernel.sysctl."net.ipv4.conf.default.send_redirects" = 0;
    boot.kernel.sysctl."net.ipv4.conf.all.accept_source_route" = 0;
    boot.kernel.sysctl."net.ipv6.conf.all.accept_source_route" = 0;
    # TCP optimization
    boot.kernel.sysctl."net.ipv4.tcp_fastopen" = 3;
    boot.kernel.sysctl."net.core.default_qdisc" = "cake";
    boot.kernel.sysctl."net.ipv4.tcp_congestion_control" = "bbr";
    boot.kernelModules = [ "tcp_bbr" ];
    # Nix optimization
    nix.autoOptimiseStore = mkDefault true;
    nix.optimise.automatic = mkDefault true;
    nix.optimise.dates = mkDefault [ "daily" ];
    # Nix garbage collector
    nix.gc.automatic = mkDefault true;
    nix.gc.dates = mkDefault "daily";
    # Remove logs
    systemd.services.rmlogs.enable = mkDefault false; # FIXME: rmlogs.service not working
  };
}