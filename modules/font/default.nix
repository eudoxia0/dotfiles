{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  fonts.enableDefaultPackages = true;

  # Custom fonts.
  fonts.packages = with pkgs; [
    dejavu_fonts
    doulos-sil
    fira-code
    gyre-fonts
    inconsolata
    liberation_ttf
    libertinus
    newcomputermodern
    nika-fonts
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    terminus_font
    terminus_font_ttf
    u001-font
  ];

  # Render fonts with anti-aliasing.
  fonts.fontconfig.antialias = true;

  # Font hinting tries to align the text outlines to the pixel grid,
  # to make text appear crisper. This is most useful in low-resolution
  # displays, so we can turn it off.
  fonts.fontconfig.hinting.enable = false;

  # Subpixel rendering exploits the physical layout of LCD screens to
  # make text look sharper. Turn it off when doing fractional display
  # scaling.
  fonts.fontconfig.subpixel.rgba = "none";
  fonts.fontconfig.subpixel.lcdfilter = "none";

  # font-related programs.
  users.users.eudoxia.packages = with pkgs; [
    font-manager
    gnome-font-viewer
  ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.eudoxia.d/bin/font-cache-update - - - - ${dotfilesDir}/modules/font/font-cache-update.sh"
    "L+ /home/eudoxia/.config/fontconfig/fonts.conf - - - - ${dotfilesDir}/modules/font/fonts.conf"
    "L+ /home/eudoxia/.local/share/fonts - - - - ${dotfilesDir}/modules/font/custom"
  ];
}
