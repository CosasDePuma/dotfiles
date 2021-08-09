{ config, lib, ... }:
with lib; {
  config = {
    # No docs, only man
    documentation.enable = mkDefault true;
    documentation.dev.enable = mkDefault false;
    documentation.doc.enable = mkDefault false;
    documentation.info.enable = mkDefault false;
    documentation.man.enable = mkDefault true;
    documentation.man.generateCaches = true;
    documentation.nixos.enable = mkDefault false;
  };
}