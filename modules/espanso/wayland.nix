{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.espanso.package = pkgs.espanso-wayland;
}
