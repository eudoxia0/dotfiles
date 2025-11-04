{
  config,
  pkgs,
  ...
}:

{
  networking.hostName = "rostam";

  home-manager.users.eudoxia.home.stateVersion = "25.05"; # DO NOT CHANGE
  system.stateVersion = "25.05"; # DO NOT CHANGE

  nixpkgs.config.allowUnfree = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices."luks-5e2a0183-ed29-499d-8741-ea27e08caf28".device =
    "/dev/disk/by-uuid/5e2a0183-ed29-499d-8741-ea27e08caf28";

  # AMD-specific.
  hardware.cpu.amd.updateMicrocode = true;

  fonts.fontconfig.defaultFonts.emoji = [ "Apple Color Emoji" ];

  home-manager.users.eudoxia.programs.git.settings.user = {
    signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIELrqPWqe1qDaTvYXyM3fw0+ToGRN6R+1qqt3QN5vWCR fernando@borretti.me";
  };

  # Tell the CPU governor to keep the CPU at the highest frequency.
  powerManagement.cpuFreqGovernor = "performance";

  # DPI.
  services.xserver.dpi = 168;
  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    QT_SCALE_FACTOR = "2";
  };
}
