{ config, pkgs, lib, ... }:

{
  # System packages.
  environment.systemPackages = with pkgs; [
    cagebreak
    clang
    curl
    gnumake
    pciutils # lspci
    (polybar.override {
      pulseSupport = true;
    })
    pulsemixer
    ruby
    rustup
    sqlite
    stow
    usbutils # lsusb
    vim
    xorg.xev
    xscreensaver
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
    ddcutil
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
    pavucontrol
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
