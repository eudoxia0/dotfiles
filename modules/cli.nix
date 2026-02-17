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
    gnuplot
    inxi
    ispell
    lsof
    monolith
    ocrmypdf
    pandoc
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
