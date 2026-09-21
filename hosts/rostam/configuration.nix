{
  config,
  pkgs,
  dotfilesDir,
  ...
}:

let
  deviceUuid = "74e2a22c-c0d3-4bfd-ac1f-3cb889b6e1ff";
in
{
  networking.hostName = "rostam";

  boot.initrd.luks.devices."luks-${deviceUuid}".device = "/dev/disk/by-uuid/${deviceUuid}";

  # Keep AMD microcode up to date.
  hardware.cpu.amd.updateMicrocode = true;

  # Radeon inspector.
  users.users.eudoxia.packages = [ pkgs.radeontop ];

  # Add your user to the necessary groups
  users.users.eudoxia.extraGroups = [
    "video"
    "render"
  ];

  # Install ROCm and related packages.
  environment.systemPackages = with pkgs; [
    rocmPackages.rocm-smi
    rocmPackages.rocminfo
    rocmPackages.clr.icd
  ];

  systemd.tmpfiles.rules = [
    "L+ /etc/sddm.conf.d/hidpi.conf - - - - ${dotfilesDir}/hosts/rostam/sddm-hidpi.conf"
    "L+ /home/eudoxia/.config/polybar/config.ini - - - - ${dotfilesDir}/hosts/rostam/polybar-rostam.ini"
    "L+ /home/eudoxia/.config/sway/extra/rostam - - - - ${dotfilesDir}/hosts/rostam/sway.conf"
  ];

  services.displayManager.ly.settings.box_title = "rostam";

  system.stateVersion = "25.05"; # DO NOT CHANGE
  nixpkgs.config.allowUnfree = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.tmp.cleanOnBoot = true;
}
