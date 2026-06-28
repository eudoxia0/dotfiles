{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia = hm: {
    home = {
      # Install alacritty.
      packages = [ pkgs.alacritty ];

      file = {
        # Symlink the config.
        ".config/alacritty/alacritty.toml".source = ./config.toml;
      };
    };
  };
}
