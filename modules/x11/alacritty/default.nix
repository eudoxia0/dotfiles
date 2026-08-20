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
    };
  };

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/alacritty/alacritty.toml - - - - /home/eudoxia/root/1-workspace/dotfiles/modules/x11/alacritty/config.toml"
  ];
}
