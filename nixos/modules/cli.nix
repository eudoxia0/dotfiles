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
    monolith
    neofetch
    pciutils # lspci
    restic
    taskwarrior-tui
    taskwarrior3
    usbutils # lsusb
  ];
}
