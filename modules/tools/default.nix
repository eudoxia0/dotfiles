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
    ".eudoxia.d/bin/count-inodes-du" = {
      source = ./count-inodes-du.py;
      executable = true;
    };
  };
}
