{ config, pkgs, lib, ... }:


{
  system.stateVersion = "25.05"; # DO NOT CHANGE
  nixpkgs.config.allowUnfree = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices."luks-5e2a0183-ed29-499d-8741-ea27e08caf28".device = "/dev/disk/by-uuid/5e2a0183-ed29-499d-8741-ea27e08caf28";
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # users
  users.users.eudoxia = {
    isNormalUser = true;
    description = "eudoxia";
    extraGroups = [ "networkmanager" "wheel" "i2c" ];
  };

  # services
  services.printing.enable = true;

  # programs
  programs.firefox.enable = true;
  programs._1password.enable = true;
  programs._1password-gui = {
    enable = true;
  };

  # home manager
  home-manager.users.eudoxia = {
    home = {
      stateVersion = "25.05"; # DO NOT CHANGE
    };
  };

  # Fix a bug where `todoist-electron` thinks the timezone is `undefined` for some reason. Instead we explicitly set `TZ`.
  nixpkgs.overlays = [(self: super: {
    todoist-electron = super.symlinkJoin {
      name = "todoist-electron";
      paths = [ super.todoist-electron ];
      buildInputs = [ super.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/todoist-electron \
          --set TZ "Australia/Sydney"
      '';
    };
  })];

  # AMD-specific.
  hardware.cpu.amd.updateMicrocode = true;

  # Ensure I have GPU drivers.
  hardware.graphics.enable = true;
}
