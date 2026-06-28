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
    max-jobs = 2;
    cores = 2;
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
      libGLU
      libGL
      libX11
      libXcursor
      libXrandr
      libXi
      libXext
      libXxf86vm
      openal
      libpulseaudio
      alsa-lib
      stdenv.cc.cc.lib
      libxkbcommon
    ];
  };
}
