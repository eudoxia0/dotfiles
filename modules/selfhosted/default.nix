{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia = {
    home.file = {
      ".config/zetanom/config.toml" = {
        text = ''
          database_path = "/home/eudoxia/Root/Databases/zetanom/zetanom.db"
          port = 12001
        '';
      };
    }
  };
}
