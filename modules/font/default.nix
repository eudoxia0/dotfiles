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

  # fontconfig settings
  fonts.fontconfig = {
    antialias = true;
    hinting = {
      enable = true;
      style = "slight";
    };
    subpixel = {
      rgba = "rgb";
      lcdfilter = "default";
    };
  };

  # font-related programs.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    font-manager
    gnome-font-viewer
  ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.eudoxia.d/bin/font-cache-update - - - - ${dotfilesDir}/modules/font/font-cache-update.sh"
    "L+ /home/eudoxia/.config/fontconfig/fonts.conf - - - - ${dotfilesDir}/modules/font/fonts.conf"
    "L+ /home/eudoxia/.local/share/fonts - - - - ${dotfilesDir}/modules/font/custom"
  ];
}
