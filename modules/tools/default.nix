{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.file = {
    ".eudoxia.d/bin/e-count-inodes" = {
      source = ./count-inodes.py;
      executable = true;
    };
    ".eudoxia.d/bin/e-count-inodes-du" = {
      source = ./count-inodes-du.py;
      executable = true;
    };
  };
}
