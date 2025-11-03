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
    fastfetch
    file
    neofetch
    pciutils # lspci
    taskwarrior-tui
    taskwarrior3
    usbutils # lsusb
  ];
}
