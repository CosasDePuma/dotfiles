{ config, lib, modulesPath, ... }: {
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot = {
    kernelModules = [ "kvm-intel" ];
    initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/SYSTEM";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/DE2D-386B";
      fsType = "vfat";
    };
  };

  swapDevices = [ { device = "/dev/disk/by-label/SWAP"; } ];

  services.xserver.dpi = 180;

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  hardware.video.hidpi.enable = lib.mkDefault true;
}
