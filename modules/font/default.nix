{
  config,
  pkgs,
  lib,
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
    noto-fonts-color-emoji
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    terminus_font
    terminus_font_ttf
    texlivePackages.times
    u001-font
    unifont
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

  # script to reload the font cache
  home-manager.users.eudoxia.home.file = {
    ".eudoxia.d/bin/font-cache-update" = {
      source = ./font-cache-update.sh;
      executable = true;
    };
  };
}
