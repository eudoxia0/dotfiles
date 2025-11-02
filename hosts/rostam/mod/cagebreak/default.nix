{ config, pkgs, lib, ... }:

{
  # cagebreak
  environment.etc."xdg/wayland-sessions/cagebreak.desktop".text = ''
      [Desktop Entry]
      Name=cagebreak
      Comment=Launch cagebreak
      Exec=${pkgs.cagebreak}/bin/cagebreak -c /home/eudoxia/.cagebreak
      Type=Application
      Keywords=Wayland;Compositor;
  '';

  home-manager.users.eudoxia.home.file.".cagebreak".source = ./cagebreak.conf;
}
