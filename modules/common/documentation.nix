{ config, ... }: {
  config = {
    documentation = {
      enable = true;
      dev.enable = false;
      doc.enable = false;
      info.enable = false;
      man.enable = true;
      man.generateCaches = true;
      nixos.enable = false;
    };
  };
}