{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.xdg.configFile."awesome/rc.lua".source = ./rc.lua;
}
