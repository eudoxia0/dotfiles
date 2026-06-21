{
  config,
  pkgs,
  lib,
  ...
}:

let
  calibre-scaled = pkgs.writeShellScriptBin "calibre-scaled" ''
    if [[ `hostname` == "rostam" ]]; then
      QT_SCALE_FACTOR=2 ${pkgs.calibre}/bin/calibre
    else
        ${pkgs.calibre}/bin/calibre
    fi
  '';
in
{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    baobab
    calibre-scaled
    cheese
    chromium
    dbeaver-bin
    djview
    evince
    file-roller
    gimp3
    gnome-calculator
    gnome-disk-utility
    gnome-pomodoro
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
    zathura
  ];

  programs._1password.enable = true;
  programs._1password-gui.enable = true;
}
