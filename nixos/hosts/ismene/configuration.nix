{ config, pkgs, ... }:

{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.initrd.luks.devices."luks-8a7825d3-550f-44be-a8a2-47ddcee57bb8".device =
    "/dev/disk/by-uuid/8a7825d3-550f-44be-a8a2-47ddcee57bb8";
  networking.hostName = "ismene";
  networking.networkmanager.enable = true;
  programs.nm-applet.enable = true;

  programs.dconf.enable = true;

  services.xserver.enable = true;
  # services.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.xfce.enable = true;

  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };


  # Tell the CPU governor to keep the CPU at the lowest frequency.
  powerManagement.cpuFreqGovernor = "powersave";

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    curl
    git
    acpi
    gnumake
  ];
  hardware.bluetooth.enable = true;
  hardware.cpu.intel.updateMicrocode = true;

  home-manager.users.eudoxia.home.stateVersion = "25.05"; # DO NOT CHANGE
  system.stateVersion = "25.05"; # DO NOT CHANGE

  fonts.packages = [ pkgs.noto-color-emoji ];
  fonts.fontconfig.defaultFonts.emoji = [ "Noto Color Emoji" ];

  home-manager.users.eudoxia.programs.git.settings.user = {
    signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIcjDwuJ2FcXmZ7RQihS65HIJQbLqjqRkzRttdLUDhrs fernando@borretti.me";
  };
}
