{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Install nushell.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    nushell
  ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/nushell/config.nu - - - - /home/eudoxia/root/1-workspace/dotfiles/modules/nushell/config.nu"
  ];
}
