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
  home-manager.users.eudoxia.xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "application/epub+zip" = [ "koreader.desktop" ];
      "application/pdf" = [ "org.gnome.Evince.desktop" ];
      "application/vnd.amazon.ebook" = [ "koreader.desktop" ];
      "application/x-fictionbook+xml" = [ "koreader.desktop" ];
      "application/x-mobipocket-ebook" = [ "koreader.desktop" ];
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = [
        "libreoffice-writer.desktop"
      ];
      "image/avif" = [ "viewnior.desktop" ];
      "image/gif" = [ "viewnior.desktop" ];
      "image/jpeg" = [ "viewnior.desktop" ];
      "image/png" = [ "viewnior.desktop" ];
      "image/svg+xml" = [ "viewnior.desktop" ];
      "image/vnd.djvu" = [ "djvulibre-djview4.desktop" ];
      "image/vnd.microsoft.icon" = [ "viewnior.desktop" ];
      "image/webp" = [ "viewnior.desktop" ];
      "image/x-djvu" = [ "djvulibre-djview4.desktop" ];
      "image/x-icon" = [ "viewnior.desktop" ];
      "text/html" = [ "firefox.desktop" ];
      "text/markdown" = [ "emacs.desktop" ];
      "text/plain" = [ "emacs.desktop" ];
      "x-scheme-handler/http" = [ "firefox.desktop" ];
      "x-scheme-handler/https" = [ "firefox.desktop" ];
    };
  };
}
