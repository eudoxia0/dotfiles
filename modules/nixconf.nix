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
    options = "--delete-older-than 7f";
  };
}
