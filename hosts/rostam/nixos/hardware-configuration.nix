{
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules = [
    "nvme"
    "xhci_pci"
    "ahci"
    "usbhid"
    "usb_storage"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/4a03e208-c7df-49bf-b738-5e00b6acc4e4";
    fsType = "ext4";
  };

  boot.initrd.luks.devices."luks-e02d823d-c198-406e-bd00-169501cbb2cd".device =
    "/dev/disk/by-uuid/e02d823d-c198-406e-bd00-169501cbb2cd";

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/5342-9D79";
    fsType = "vfat";
    options = [
      "fmask=0077"
      "dmask=0077"
    ];
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/f56b24e9-2caf-4412-86e6-f4843e49a0bc"; }
  ];

  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
