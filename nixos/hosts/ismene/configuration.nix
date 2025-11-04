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

  services.xserver.enable = true;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.desktopManager.mate.enable = true;
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

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    curl
    gnumake
    git
  ];
  hardware.bluetooth.enable = true;

  home-manager.users.eudoxia.home.stateVersion = "25.05"; # DO NOT CHANGE
  system.stateVersion = "25.05"; # DO NOT CHANGE
}
