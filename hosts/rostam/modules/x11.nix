{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.xserver.enable = true;
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };
  services.xserver.dpi = 168;
  environment.variables = {
    GDK_SCALE = "2";
    GDK_DPI_SCALE = "0.5";
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    QT_SCALE_FACTOR = "2";
  };
  home-manager.users.eudoxia.home.packages = with pkgs; [
    scrot
    arandr
    xfce.thunar
    xorg.xev
  ];
}
