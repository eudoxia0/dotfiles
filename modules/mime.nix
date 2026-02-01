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

let
  # image-viewer = "org.xfce.ristretto.desktop";
  image-viewer = "viewnior.desktop";
in
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
        "writer.desktop"
      ];
      "image/avif" = [ image-viewer ];
      "image/gif" = [ image-viewer ];
      "image/jpeg" = [ image-viewer ];
      "image/png" = [ image-viewer ];
      "image/svg+xml" = [ image-viewer ];
      "image/vnd.djvu" = [ "djvulibre-djview4.desktop" ];
      "image/vnd.microsoft.icon" = [ image-viewer ];
      "image/webp" = [ image-viewer ];
      "image/x-djvu" = [ "djvulibre-djview4.desktop" ];
      "image/x-icon" = [ image-viewer ];
      "text/html" = [ "firefox.desktop" ];
      "text/markdown" = [ "emacs.desktop" ];
      "text/plain" = [ "emacs.desktop" ];
      "text/xml" = [ "emacs.desktop" ];
      "application/xml" = [ "emacs.desktop" ];
      "x-scheme-handler/http" = [ "firefox.desktop" ];
      "x-scheme-handler/https" = [ "firefox.desktop" ];
    };
  };
}
