{
  config,
  pkgs,
  lib,
  ...
}:

{
  # User packages.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    age
    calibre
    chromium
    fastfetch
    flowtime
    foot
    gargoyle
    gimp3
    ideogram
    just
    libreoffice
    mupdf
    neofetch
    nwg-look
    pciutils # lspci
    seahorse
    signal-desktop
    taskwarrior-tui
    taskwarrior3
    typst
    usbutils # lsusb
    wdisplays
    zed-editor
  ];
}
