# MIME configuration
#
# This isn't the best way to do this. Ideally, each application should be its
# own module, and that module should both install the package and configure
# itself as the owner of whatever MIME types it opens.

{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/pdf" = ["evince.desktop"];
    };
  };
}
