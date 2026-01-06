{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.file = {
    ".eudoxia.d/bin/add-to-dictionary" = {
      source = ./add-to-dictionary.py;
      executable = true;
    };
    ".eudoxia.d/bin/add-feed" = {
      source = ./add-feed.py;
      executable = true;
    };
    ".eudoxia.d/bin/timestamp" = {
      source = ./timestamp.py;
      executable = true;
    };
    ".eudoxia.d/bin/find-syncthing-conflicts" = {
      source = ./find-syncthing-conflicts.py;
      executable = true;
    };
  };
}
