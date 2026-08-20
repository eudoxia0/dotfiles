{
  config,
  pkgs,
  lib,
  dotfilesDir,
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
    "L+ /home/eudoxia/.config/alacritty/alacritty.toml - - - - ${dotfilesDir}/modules/x11/alacritty/config.toml"
  ];
}
