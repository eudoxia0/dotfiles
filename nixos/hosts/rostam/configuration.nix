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

  # DPI settings. On my 4K monitor, the native DPI is 96. A DPI of 168
  # is 96 * 1.75. The effective resolution is 2160 (the height in px of a 4K
  # monitor) times the reciprocal of 1.75, i.e., 2160/1.75 = ~1200. So it's a
  # bit bigger than 1080p, a bit smaller than 1440p.
  services.xserver.dpi = 168;
  environment.variables = {
    # Make the GUI 2x bigger. Sadly this only takes integer values.
    GDK_SCALE = "2";
    # Scales text only. Since `GDK_SCALE` is scaling everything by 2x, scaling
    # down by 0.5 works out to keeping the text at the same size.
    GDK_DPI_SCALE = "0.5";
  };

  nixpkgs.overlays = [
    (self: super: {
      obsidian = super.symlinkJoin {
        name = "obsidian";
        paths = [ super.obsidian ];
        buildInputs = [ super.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/obsidian \
            --add-flags "--force-device-scale-factor=1.5"
        '';
      };

      signal-desktop = super.symlinkJoin {
        name = "signal-desktop";
        paths = [ super.signal-desktop ];
        buildInputs = [ super.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/signal-desktop \
            --add-flags "--force-device-scale-factor=1.5"
        '';
      };

      # This also fixes a bug where `todoist-electron` thinks the timezone is
      # `undefined` for some reason. Instead we explicitly set `TZ`.
      todoist-electron = super.symlinkJoin {
        name = "todoist-electron";
        paths = [ super.todoist-electron ];
        buildInputs = [ super.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/todoist-electron \
            --add-flags "--force-device-scale-factor=1.5"
            --set TZ "Australia/Sydney"
        '';
      };
    })
  ];

  # Keep AMD microcode up to date.
  hardware.cpu.amd.updateMicrocode = true;
}
