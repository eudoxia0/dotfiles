{ config, pkgs, ... }:

let
  deviceUuid = "5e2a0183-ed29-499d-8741-ea27e08caf28";
in
{
  networking.hostName = "rostam";

  boot.initrd.luks.devices."luks-${deviceUuid}".device = "/dev/disk/by-uuid/${deviceUuid}";

  fonts.fontconfig.defaultFonts.emoji = [ "Apple Color Emoji" ];

  home-manager.users.eudoxia.programs.git.settings.user = {
    signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIELrqPWqe1qDaTvYXyM3fw0+ToGRN6R+1qqt3QN5vWCR fernando@borretti.me";
  };

  # Tell the CPU governor to keep the CPU at the highest frequency.
  powerManagement.cpuFreqGovernor = "performance";

  # DPI settings.
  services.xserver.dpi = 168;
  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
  };

  # Keep AMD microcode up to date.
  hardware.cpu.amd.updateMicrocode = true;
}
