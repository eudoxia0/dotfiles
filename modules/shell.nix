{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.bash.enable = true;

  # Add directories to PATH.
  environment.sessionVariables = {
    PATH = [
      "/home/eudoxia/.eudoxia.d/bin"
      "/home/eudoxia/.cargo/bin"
    ];
  };
}
