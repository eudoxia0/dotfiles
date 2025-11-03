{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    calibre
    chromium
    flowtime
    foot
    gargoyle
    gimp3
    ideogram
    libreoffice
    mupdf
    nwg-look # like a modern lxappearance
    seahorse
    signal-desktop
    wdisplays
    gnumeric
    file-roller
  ];
}
