{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  users.users.eudoxia.packages = with pkgs; [
    waybar
  ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/waybar/config.jsonc - - - - ${dotfilesDir}/modules/wayland/waybar/waybar.jsonc"
    "L+ /home/eudoxia/.config/waybar/style.css - - - - ${dotfilesDir}/modules/wayland/waybar/style.css"
  ];
}
