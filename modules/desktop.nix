{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.ydotool.enable = true;
  users.users.eudoxia.extraGroups = [ "ydotool" ];

  home-manager.users.eudoxia.home.packages = with pkgs; [
    appimage-run
    baobab
    bemoji
    calibre
    cheese
    chromium
    dbeaver-bin
    djview
    evince
    file-roller
    gimp3
    gnome-disk-utility
    gparted
    ideogram
    imv
    keepassxc
    koreader
    libreoffice
    mpv
    mupdf
    nwg-look # like a modern lxappearance
    obsidian
    protonmail-desktop
    quodlibet
    seahorse
    signal-desktop
    strawberry
    sxiv
    transmission_4-gtk
    v4l-utils
    viewnior
    vlc
    vscode
    wdisplays
    wev
    wtype
    xfce.ristretto
    zathura
  ];

  nixpkgs.overlays = [
    (self: super: {
      signal-desktop = super.symlinkJoin {
        name = "signal-desktop";
        paths = [ super.signal-desktop ];
        buildInputs = [ super.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/signal-desktop \
            --add-flags "--enable-features=UseOzonePlatform --ozone-platform-hint=auto"
        '';
      };
    })
  ];
}
