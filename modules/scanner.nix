{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Enable SANE scanners.
  hardware.sane.enable = true;
}
