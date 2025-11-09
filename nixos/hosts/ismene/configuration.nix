{ config, pkgs, ... }:

let
  deviceUuid = "8a7825d3-550f-44be-a8a2-47ddcee57bb8";
in
{
  networking.hostName = "ismene";

  boot.initrd.luks.devices."luks-${deviceUuid}".device = "/dev/disk/by-uuid/${deviceUuid}";

  # Tell the CPU governor to keep the CPU at the lowest frequency.
  powerManagement.cpuFreqGovernor = "powersave";

  hardware.bluetooth.enable = true;

  fonts.packages = [ pkgs.noto-fonts-color-emoji ];
  fonts.fontconfig.defaultFonts.emoji = [ "Noto Color Emoji" ];

  home-manager.users.eudoxia.programs.git.settings.user = {
    signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIcjDwuJ2FcXmZ7RQihS65HIJQbLqjqRkzRttdLUDhrs fernando@borretti.me";
  };

  # Keep Intel microcode up to date.
  hardware.cpu.intel.updateMicrocode = true;

  # Todoist overlay.
  nixpkgs.overlays = [
    (self: super: {
      todoist-electron = super.symlinkJoin {
        name = "todoist-electron";
        paths = [ super.todoist-electron ];
        buildInputs = [ super.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/todoist-electron \
            --set TZ "Australia/Sydney"
        '';
      };
    })
  ];

  # Syncthing config.
  home-manager.users.eudoxia.services.syncthing = {
    settings.devices = {
      rostam = {
        name = "rostam";
        id = "H6XRBBB-7WXJPGN-GVMFX3T-BANURMZ-B5CYCIZ-ZMVNFEB-HQWGIXE-3AZ5EQ4";
      };
    };
    settings.folders = {
      "root" = {
        id = "root";
        label = "Root";
        path = "/home/eudoxia/Root";
        devices = [ "rostam" ];
      };
    };
  };

  environment.systemPackages = with pkgs; [
    brightnessctl
  ];
}
