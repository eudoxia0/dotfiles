{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Experimental Nix features.
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # Speed up compilation.
  nix.settings = {
    max-jobs = "auto";
    cores = 0;
  };

  # Use hard links for identical paths.
  nix.settings.auto-optimise-store = true;

  # Automatic GC.
  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 7d";
  };

  # Stop warning about dirty trees.
  nix.settings.warn-dirty = false;

  # Cache flake evaluation.
  nix.settings.eval-cache = true;

  # Enable the nix-ld shim.
  programs.nix-ld = {
    enable = true;
    libraries = with pkgs; [
      # alsa-lib
      # libGL
      # libGLU
      # libX11
      # libXcursor
      # libXext
      # libXi
      # libXmu
      # libXrandr
      # libXxf86vm
      # libpulseaudio
      # libxkbcommon
      # openal
      # stdenv.cc.cc.lib
    ];
  };

  # Gaming type shi.
  hardware.graphics.enable = true;
  hardware.graphics.enable32Bit = true;
}
