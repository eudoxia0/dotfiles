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

  # Keep AMD microcode up to date.
  hardware.cpu.amd.updateMicrocode = true;

  # Syncthing config.
  home-manager.users.eudoxia.services.syncthing = {
    settings.devices = {
      ismene = {
        name = "ismene";
        id = "5H3EV2X-SOOZJKN-RHNNJUQ-I6HGKQS-WBT5EE2-N5O7L6O-PGEVWLF-WZB3CQO";
      };
    };
    settings.folders = {
      "root" = {
        id = "root";
        label = "Root";
        path = "/home/eudoxia/Root";
        devices = [ "ismene" ];
      };
    };
  };

  environment.variables = {
    GDK_DPI_SCALE = "1.5";
  };

  # Radeon inspector.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    radeontop
  ];

  # Add your user to the necessary groups
  users.users.eudoxia.extraGroups = [ "video" "render" ];

  # Install ROCm and related packages.
  environment.systemPackages = with pkgs; [
    rocmPackages.rocm-smi
    rocmPackages.rocminfo
    rocmPackages.clr.icd
  ];
}
