{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.programs.gh = {
    enable = true;
    settings = {
      editor = "emacs";
      git_protocol = "ssh";
    };
  };
}
