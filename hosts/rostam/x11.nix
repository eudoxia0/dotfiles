{
  config,
  pkgs,
  dotfilesDir,
  ...
}:

{
  # X11 DPI. Values: 96, 144, 192.
  services.xserver.dpi = 144;

  # Monitor scaling.
  environment.sessionVariables = {
    # Scale GTK apps. Integer.
    GDK_SCALE = "2";
    # Scale GTK font sizes. Real number.
    GDK_DPI_SCALE = "0.5";
    # Scale GT apps. Real number.
    QT_SCALE_FACTOR = "2";
  };

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/polybar/config.ini - - - - ${dotfilesDir}/hosts/rostam/polybar-rostam.ini"
  ];
}
