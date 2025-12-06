{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    age
    beets
    djvu2pdf
    file
    inxi
    ispell
    lsof
    monolith
    ocrmypdf
    pciutils # lspci
    ripgrep
    smartmontools
    taskwarrior-tui
    taskwarrior3
    termdown
    tt
    usbutils # lsusb
    yt-dlp
  ];
}
