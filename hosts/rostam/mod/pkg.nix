{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    age
    fastfetch
    neofetch
    pciutils # lspci
    taskwarrior-tui
    taskwarrior3
    typst
    usbutils # lsusb
  ];
}
