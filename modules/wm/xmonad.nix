{ pkgs, ... }: {
  services.xserver = {
    enable = true;
    autorun = true;
    displayManager.defaultSession = "none+xmonad";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };

  environment.systemPackages = with pkgs; [
    feh
    fira-code
    haskellPackages.xmobar
    kitty
    rofi
    xorg.xmessage
  ];
}