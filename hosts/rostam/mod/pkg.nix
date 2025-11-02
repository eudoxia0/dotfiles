{ config, pkgs, lib, ... }:

{
  # System packages.
  environment.systemPackages = with pkgs; [
    clang
    curl
    gnumake
    pciutils # lspci
    ruby
    sqlite
    stow
    usbutils # lsusb
    vim
    xorg.xev
  ];
  environment.localBinInPath = true;

  # User packages.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    agda
    age
    arandr
    btop
    calibre
    chromium
    fastfetch
    fd
    feh
    flowtime
    foot
    gimp3
    guile
    ideogram
    imagemagick
    inform7
    just
    libreoffice
    mate.caja
    neofetch
    nwg-look
    pcmanfm
    rox-filer
    seahorse
    signal-desktop
    taskwarrior-tui
    taskwarrior3
    todoist-electron
    typst
    wdisplays
    xcape
    xfce.thunar
    zed-editor
  ];
}
