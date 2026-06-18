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
        ".config/alacritty/alacritty.toml".source =
          hm.config.lib.file.mkOutOfStoreSymlink "${hm.config.home.homeDirectory}/root/1-workspace/dotfiles/modules/x11/alacritty/config.toml";
      };
    };
  };
}
