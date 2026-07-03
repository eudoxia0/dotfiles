{ config, pkgs, ... }:

let
  deviceUuid = "a4725842-ece5-467d-9598-6583ed21c7eb";
in
{
  networking.hostName = "ismene";

  boot.initrd.luks.devices."luks-${deviceUuid}".device = "/dev/disk/by-uuid/${deviceUuid}";

  # home-manager.users.eudoxia.programs.git.settings.user = {
  #  signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIcjDwuJ2FcXmZ7RQihS65HIJQbLqjqRkzRttdLUDhrs fernando@borretti.me";
  # };

  # Keep Intel microcode up to date.
  hardware.cpu.intel.updateMicrocode = true;

  environment.systemPackages = with pkgs; [
    acpi
    bluetui
  ];

  services.thermald.enable = true;

  # Power management.
  services.auto-cpufreq.enable = true;
  services.auto-cpufreq.settings = {
    battery = {
      governor = "powersave";
      turbo = "never";
    };
    charger = {
      governor = "performance";
      turbo = "always";
    };
  };

  home-manager.users.eudoxia.home.stateVersion = "25.11"; # DO NOT CHANGE
  system.stateVersion = "25.11"; # DO NOT CHANGE
  nixpkgs.config.allowUnfree = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.tmp.cleanOnBoot = true;
}
