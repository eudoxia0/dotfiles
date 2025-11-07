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
    inxi
    monolith
    neofetch
    pciutils # lspci
    restic
    taskwarrior-tui
    taskwarrior3
    usbutils # lsusb
  ];
}
