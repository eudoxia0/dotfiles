{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.xserver.windowManager.awesome.enable = true;

  home-manager.users.eudoxia.xdg.configFile."awesome/rc.lua".source = ./rc.lua;
}
