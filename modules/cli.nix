{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    age
    djvu2pdf
    file
    inxi
    monolith
    pciutils # lspci
    restic
    silver-searcher
    taskwarrior-tui
    taskwarrior3
    termdown
    usbutils # lsusb
  ];
}
