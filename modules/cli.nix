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
    restic
    ripgrep
    rsync
    smartmontools
    termdown
    tesseract
    tt
    usbutils # lsusb
    yt-dlp
  ];
}
