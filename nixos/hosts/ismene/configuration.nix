{ config, pkgs, ... }:

{
  networking.hostName = "ismene";

  boot.initrd.luks.devices."luks-8a7825d3-550f-44be-a8a2-47ddcee57bb8".device =
    "/dev/disk/by-uuid/8a7825d3-550f-44be-a8a2-47ddcee57bb8";

  programs.dconf.enable = true;

  # Tell the CPU governor to keep the CPU at the lowest frequency.
  powerManagement.cpuFreqGovernor = "powersave";

  hardware.bluetooth.enable = true;
  hardware.cpu.intel.updateMicrocode = true;

  fonts.packages = [ pkgs.noto-fonts-color-emoji ];
  fonts.fontconfig.defaultFonts.emoji = [ "Noto Color Emoji" ];

  home-manager.users.eudoxia.programs.git.settings.user = {
    signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIcjDwuJ2FcXmZ7RQihS65HIJQbLqjqRkzRttdLUDhrs fernando@borretti.me";
  };
}
