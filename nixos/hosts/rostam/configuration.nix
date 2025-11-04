{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.stateVersion = "25.05"; # DO NOT CHANGE
  system.stateVersion = "25.05"; # DO NOT CHANGE

  nixpkgs.config.allowUnfree = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices."luks-5e2a0183-ed29-499d-8741-ea27e08caf28".device =
    "/dev/disk/by-uuid/5e2a0183-ed29-499d-8741-ea27e08caf28";

  # Speed up the boot process.
  boot.loader.timeout = 2;

  environment.localBinInPath = true;

  # users
  users.users.eudoxia = {
    isNormalUser = true;
    description = "eudoxia";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
  };

  # services
  services.printing.enable = true;

  # AMD-specific.
  hardware.cpu.amd.updateMicrocode = true;

  # Ensure I have GPU drivers.
  hardware.graphics.enable = true;
}
