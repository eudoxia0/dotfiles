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

  systemd.tmpfiles.rules = [
    # Miranda-specific Sway config.
    "L+ /home/eudoxia/.config/sway/extra/miranda - - - - ${dotfilesDir}/hosts/miranda/sway.conf"
  ];
}
