# FIXME: rmlogs.service: Service has no ExecStart=, ExecStop=, or SuccessAction=. Refusing.

{ config, lib, pkgs, ... }:
with lib;
let
  name = "rmlogs";
  cfg = config.services.${name}; 
in {
  options = {
    services.${name} = {
      enable = mkEnableOption "Remove logs";
    };
  };

  config = mkIf cfg.enable {
    systemd = {
      services.${name} = {
        description = "Remove logs periodically";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.systemd}/bin/journalctl --vacuum-time=30d";
        };
        timers.${name} = {
          wantedBy = [ "timers.target" ];
          partOf = [ "${name}.service" ];
          timerConfig.OnCalendar = "weekly UTC";
        };
      };
    };
  };
}