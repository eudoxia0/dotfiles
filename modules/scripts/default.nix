{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.file = {
    ".eudoxia.d/bin/add-to-dictionary".source = ./add-to-dictionary.py;
    ".eudoxia.d/bin/add-flashcard".source = ./add-flashcard.py;
  };
}
