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
    silver-searcher
    taskwarrior-tui
    taskwarrior3
    termdown
    usbutils # lsusb
  ];
}
