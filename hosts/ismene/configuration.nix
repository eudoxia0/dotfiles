{ config, pkgs, ... }:

let
  deviceUuid = "8a7825d3-550f-44be-a8a2-47ddcee57bb8";
in
{
  networking.hostName = "ismene";

  # Zed editor configuration.
  custom.zed.fontSize = 10;

  boot.initrd.luks.devices."luks-${deviceUuid}".device = "/dev/disk/by-uuid/${deviceUuid}";

  fonts.packages = [ pkgs.noto-fonts-color-emoji ];
  fonts.fontconfig.defaultFonts.emoji = [ "Noto Color Emoji" ];

  home-manager.users.eudoxia.programs.git.settings.user = {
    signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIcjDwuJ2FcXmZ7RQihS65HIJQbLqjqRkzRttdLUDhrs fernando@borretti.me";
  };

  # Keep Intel microcode up to date.
  hardware.cpu.intel.updateMicrocode = true;

  # Syncthing config.
  home-manager.users.eudoxia.services.syncthing = {
    settings.devices = {
      rostam = {
        name = "rostam";
        id = "C7BIL55-GT5ILGN-7IOEBXJ-S3GURF2-V6JASA3-XBY3VKH-LK7GCIR-TJZ5GAH";
      };
    };
    settings.folders = {
      "root" = {
        id = "root";
        label = "Root";
        path = "/home/eudoxia/Root";
        devices = [ "rostam" ];
      };
    };
  };

  environment.systemPackages = with pkgs; [
    brightnessctl
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
      turbo = "auto";
    };
  };

  home-manager.users.eudoxia.home.stateVersion = "25.05"; # DO NOT CHANGE
  system.stateVersion = "25.05"; # DO NOT CHANGE
  nixpkgs.config.allowUnfree = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.tmp.cleanOnBoot = true;
}
