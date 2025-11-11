{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.file = {
    ".eudoxia.d/bin/count-inodes" = {
      source = ./count-inodes.py;
      executable = true;
    };
  };
}
