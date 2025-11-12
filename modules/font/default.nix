{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Don't enable the default NixOS fonts. Mostly because this includes Noto
  # emojis.
  fonts.enableDefaultPackages = false;

  # Custom fonts.
  fonts.packages = with pkgs; [
    dejavu_fonts
    fira-code
    freefont_ttf
    gyre-fonts
    inconsolata
    iosevka
    jetbrains-mono
    liberation_ttf
    nika-fonts
    noto-fonts
    noto-fonts-cjk-sans
    terminus_font
    terminus_font_ttf
    texlivePackages.times
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
    ".eudoxia.d/bin/e-font-cache-update" = {
      source = ./font-cache-update.sh;
      executable = true;
    };
  };
}
