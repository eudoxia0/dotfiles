{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ./root.nix
  ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/da4c6b80-b8c0-4d0f-ae23-7f102474c700";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."luks-33a4393c-6204-4030-8e6f-22aedd34da89".device = "/dev/disk/by-uuid/33a4393c-6204-4030-8e6f-22aedd34da89";

  fileSystems."/boot/efi" =
    { device = "/dev/disk/by-uuid/B7D0-31D7";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/7effe3a1-21d2-4f1e-b4f0-f48fe563fdba"; }
    ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp9s0.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  # high-resolution display
  hardware.video.hidpi.enable = lib.mkDefault true;

  # Enable swap on luks
  boot.initrd.luks.devices."luks-8731a1f4-b3ab-4db5-a669-be2bf61e8a2b".device = "/dev/disk/by-uuid/8731a1f4-b3ab-4db5-a669-be2bf61e8a2b";
  boot.initrd.luks.devices."luks-8731a1f4-b3ab-4db5-a669-be2bf61e8a2b".keyFile = "/crypto_keyfile.bin";

  networking.hostName = "sextant";

  services.xserver.videoDrivers = [ "intel" ];
  services.xserver.deviceSection = ''
    Option "DRI" "2"
    Option "TearFree" "true"
  '';
}
