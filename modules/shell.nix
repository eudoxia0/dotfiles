{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.bash.enable = true;

  # Add directories to PATH.
  environment.variables.PATH = [
    "/home/eudoxia/.eudoxia.d/bin"
    "/home/eudoxia/.cargo/bin"
  ];
}
