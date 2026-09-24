{
  config,
  pkgs,
  ...
}:

{
  # Monitor scaling.
  environment.sessionVariables = {
    GDK_SCALE = "1";
    GDK_DPI_SCALE = "1";
    QT_SCALE_FACTOR = "1";
  };

  services.xserver.dpi = 144;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };
}
