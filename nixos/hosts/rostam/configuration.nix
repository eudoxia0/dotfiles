{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.stateVersion = "25.05"; # DO NOT CHANGE
  system.stateVersion = "25.05"; # DO NOT CHANGE

  nixpkgs.config.allowUnfree = true;

  networking.hostName = "rostam";
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices."luks-5e2a0183-ed29-499d-8741-ea27e08caf28".device =
    "/dev/disk/by-uuid/5e2a0183-ed29-499d-8741-ea27e08caf28";

  environment.localBinInPath = true;

  # AMD-specific.
  hardware.cpu.amd.updateMicrocode = true;

  home-manager.users.eudoxia.programs.git.settings.user = {
    signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIELrqPWqe1qDaTvYXyM3fw0+ToGRN6R+1qqt3QN5vWCR fernando@borretti.me";
  };
}
