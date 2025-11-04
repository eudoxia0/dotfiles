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
    evince
    file-roller
    flowtime
    foot
    gargoyle
    gimp3
    gnucash
    gnumeric
    gnumeric
    gparted
    ideogram
    imv
    koreader
    libreoffice
    mpv
    mupdf
    nwg-look # like a modern lxappearance
    obsidian
    seahorse
    signal-desktop
    sxiv
    viewnior
    wdisplays
    zathura
  ];
}
