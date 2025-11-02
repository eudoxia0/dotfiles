{
  config,
  pkgs,
  lib,
  ...
}:

{
  # System packages.
  environment.systemPackages = with pkgs; [
    clang
    curl
    gnumake
    ruby
    sqlite
    vim
  ];
  environment.localBinInPath = true;

  # User packages.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    agda
    age
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
    neofetch
    nixfmt-tree
    nwg-look
    pciutils # lspci
    seahorse
    signal-desktop
    taskwarrior-tui
    taskwarrior3
    todoist-electron
    tokei
    typst
    usbutils # lsusb
    wdisplays
    zed-editor
  ];
}
