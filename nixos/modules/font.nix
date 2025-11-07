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

  # But do enable GS fonts.
  fonts.enableGhostscriptFonts = true;

  # Custom fonts.
  fonts.packages = with pkgs; [
    dejavu_fonts
    fira-code
    gyre-fonts
    inconsolata
    iosevka
    jetbrains-mono
    liberation_ttf
    noto-fonts
    noto-fonts-cjk-sans
    terminus_font
    terminus_font_ttf
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
    ghostscript
    gnome-font-viewer
  ];
}
