{ config, lib, pkgs, ... }:
let
  name = "zsh";
  cfg = config.software."${name}";
in {
  options = {
    software."${name}" = {
      enable = lib.mkEnableOption "custom ${name} (shell)";

      package = lib.mkPackageOption pkgs name {};

      config = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        example = ../../config/.${name}rc;
        description = "The path of the ${name} configuration file.";
      };

      history = lib.mkOption {
        type = lib.types.nonEmptyStr;
        default = "$HOME/.zsh_history";
        description = "${name} history file location.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      enableCompletion = true;
      histFile = cfg.history;
      histSize = 2000;
      promptInit = if (cfg.config != null) then builtins.readFile cfg.config else "";
      setOptions = [
        "AUTO_CD"
        "CD_SILENT"
        "HIST_FCNTL_LOCK"
        "HIST_FIND_NO_DUPS"
        "HIST_IGNORE_DUPS"
        "SHARE_HISTORY"
      ];
      syntaxHighlighting.enable = true;
    };
    users.defaultUserShell = cfg.package;
  };
}
