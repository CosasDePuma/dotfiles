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

  fonts.fonts = with pkgs; [ nerdfonts ];

  environment.systemPackages = with pkgs; [
    feh
    haskellPackages.xmobar
    kitty
    rofi
    xorg.xmessage
  ];
}