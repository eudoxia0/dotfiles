{ config, pkgs, lib, ... }:

{
  # System packages.
  environment.systemPackages = with pkgs; [
    clang
    curl
    gnumake
    ruby
    sqlite
    stow
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
    pciutils # lspci
    pcmanfm
    rox-filer
    seahorse
    signal-desktop
    taskwarrior-tui
    taskwarrior3
    todoist-electron
    typst
    usbutils # lsusb
    wdisplays
    xfce.thunar
    zed-editor
  ];
}
