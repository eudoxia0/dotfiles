{
  config,
  pkgs,
  dotfilesDir,
  ...
}:

{
  # Monitor scaling.
  environment.sessionVariables = {
    # Scale GTK apps. Integer.
    GDK_SCALE = "1";
    # Scale GTK font sizes. Real number.
    GDK_DPI_SCALE = "1";
    # Scale GT apps. Real number.
    QT_SCALE_FACTOR = "1";
  };

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/sway/extra/rostam - - - - ${dotfilesDir}/hosts/rostam/sway.conf"
  ];
}
