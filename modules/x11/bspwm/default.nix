{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.xserver.windowManager.bspwm = {
    enable = true;
    sxhkd.package = pkgs.sxhkd;
  };

  home-manager.users.eudoxia.home.packages = with pkgs; [
    feh
  ];

  home-manager.users.eudoxia.home.file = {
    ".config/bspwm/bspwmrc" = {
      source = ./bspwmrc;
      executable = true;
    };
    ".config/sxhkd/sxhkdrc".source = ./sxhkdrc;
  };
}
