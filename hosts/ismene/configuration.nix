{ config, pkgs, ... }:

let
  deviceUuid = "a4725842-ece5-467d-9598-6583ed21c7eb";
in
{
  networking.hostName = "ismene";

  boot.initrd.luks.devices."luks-${deviceUuid}".device = "/dev/disk/by-uuid/${deviceUuid}";

  # Keep Intel microcode up to date.
  hardware.cpu.intel.updateMicrocode = true;

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/polybar/config.ini - - - - ${dotfilesDir}/hosts/ismene/polybar-ismene.ini"
  ];

  system.stateVersion = "25.11"; # DO NOT CHANGE
  nixpkgs.config.allowUnfree = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.tmp.cleanOnBoot = true;
}
