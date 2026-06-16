{
  config,
  pkgs,
  lib,
  ...
}:

let
  calibre-scaled = pkgs.writeShellScriptBin "calibre-scaled" ''
    if [[ `hostname` == "rostam" ]]; then
      QT_SCALE_FACTOR=2 calibre
    else
        calibre
    fi
  '';
in
{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    baobab
    calibre
    calibre-scaled
    cheese
    chromium
    dbeaver-bin
    djview
    evince
    file-roller
    gimp3
    gnome-disk-utility
    gparted
    keepassxc
    koreader
    libreoffice
    mpv
    mupdf
    nwg-look
    obsidian
    protonmail-desktop
    quodlibet
    seahorse
    signal-desktop
    strawberry
    transmission_4-gtk
    v4l-utils
    viewnior
    vlc
    wev
    zathura
  ];
}
