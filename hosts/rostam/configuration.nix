{ config, pkgs, ... }:

let
  deviceUuid = "74e2a22c-c0d3-4bfd-ac1f-3cb889b6e1ff";
in
{
  networking.hostName = "rostam";

  # Zed editor configuration.
  custom.zed.fontSize = 14;

  boot.initrd.luks.devices."luks-${deviceUuid}".device = "/dev/disk/by-uuid/${deviceUuid}";

  fonts.fontconfig.defaultFonts.emoji = [ "Apple Color Emoji" ];

  home-manager.users.eudoxia.programs.git.settings.user = {
    signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILdw0/4AIQ4oAlhsFVTOix5ke+7iBgql2xHUEZxoPa3U fernando@borretti.me";
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
        label = "root";
        path = "/home/eudoxia/root";
        devices = [ "ismene" ];
      };
    };
  };

  environment.variables = {
    GDK_DPI_SCALE = "1.5";
    QT_SCALE_FACTOR = "1.5";
  };

  # Radeon inspector.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    radeontop
  ];

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

  services.displayManager.ly.settings.box_title = "rostam";

  home-manager.users.eudoxia.home.stateVersion = "25.05"; # DO NOT CHANGE
  system.stateVersion = "25.05"; # DO NOT CHANGE
  nixpkgs.config.allowUnfree = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.tmp.cleanOnBoot = true;
}
