# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  config,
  pkgs,
  dotfilesDir,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices."luks-b3e5bbdb-0bd6-415a-b46e-9c1de6e288bc".device =
    "/dev/disk/by-uuid/b3e5bbdb-0bd6-415a-b46e-9c1de6e288bc";
  networking.hostName = "miranda"; # Define your hostname.

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  services.xserver.dpi = 144;

  # Monitor scaling.
  environment.sessionVariables = {
    GDK_SCALE = "1";
    GDK_DPI_SCALE = "1";
    QT_SCALE_FACTOR = "1";
  };

  # Enable the GNOME Desktop Environment.
  # services.xserver.displayManager.gdm.enable = true;
  services.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Install firefox.
  programs.firefox.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    acpi
    bluetui
  ];

  # Power management.
  services.power-profiles-daemon.enable = false;
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

  systemd.tmpfiles.rules = [
    "L+ /etc/sddm.conf.d/hidpi.conf - - - - ${dotfilesDir}/hosts/miranda/sddm-hidpi.conf"
  ];

  system.stateVersion = "26.05"; # DO NOT CHANGE

}
