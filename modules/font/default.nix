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
    freefont_ttf
    gyre-fonts
    inconsolata
    iosevka
    jetbrains-mono
    liberation_ttf
    libertinus
    newcomputermodern
    nika-fonts
    noto-fonts
    noto-fonts-cjk-sans
    font-awesome
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
