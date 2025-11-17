{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia = {
    home.file = {
      ".config/zetanom/config.toml".source = ./zetanom.toml;
    };
  };
}
