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
    djview
    eog
    evince
    file-roller
    flowtime
    foot
    gargoyle
    gimp3
    gnumeric
    ideogram
    libreoffice
    mupdf
    nwg-look # like a modern lxappearance
    seahorse
    signal-desktop
    sxiv
    viewnior
    wdisplays
    zathura
  ];
}
