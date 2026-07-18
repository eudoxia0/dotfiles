{
  config,
  pkgs,
  ...
}:

let
  calibre-scaled = pkgs.writeShellScriptBin "calibre-scaled" ''
    if [[ `hostname` == "rostam" ]]; then
      QT_SCALE_FACTOR=2 ${pkgs.calibre}/bin/calibre
    else
        ${pkgs.calibre}/bin/calibre
    fi
  '';

  todoist = pkgs.writeShellScriptBin "todoist" ''
    TZ=Australia/Sydney exec ${pkgs.todoist-electron}/bin/todoist-electron "$@"
  '';

  # See: https://github.com/NixOS/nixpkgs/issues/519484
  #
  # TODO: remove when we have pandoc 3.8 or above in nixpkgs
  patchedQuarto = pkgs.quarto.overrideAttrs (oldAttrs: {
    postPatch = (oldAttrs.postPatch or "") + ''
      substituteInPlace bin/quarto.js \
        --replace-fail "syntax-highlighting" "highlight-style"
    '';
  });
in
{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    age
    baobab
    btop
    calibre-scaled
    cargo-deny
    cargo-machete
    cargo-watch
    cheese
    chromium
    clang
    claude-code
    curl
    dbeaver-bin
    djview
    djvu2pdf
    evince
    fd
    file
    file-roller
    ghostscript
    gimp3
    gnome-calculator
    gnome-disk-utility
    gnome-pomodoro
    gnumake
    gnuplot
    gparted
    graphviz
    imagemagick
    inxi
    ispell
    jekyll
    jq
    just
    keepassxc
    libheif
    libreoffice
    libxml2 # xmllint
    lsof
    monolith
    mpv
    nixfmt-tree
    nwg-look
    obsidian
    ocrmypdf
    pandoc
    patchedQuarto
    pciutils # lspci
    poppler-utils
    python314
    quodlibet
    restic
    ripgrep
    rsync
    ruff
    rustup
    sass
    seahorse
    signal-desktop
    smartmontools
    sqlite
    strawberry
    termdown
    tesseract
    texliveFull
    todoist
    tokei
    transmission_4-gtk
    tree
    tt
    typst
    usbutils # lsusb
    uv
    viewnior
    vlc
    yt-dlp
    zathura
    zed-editor
    zola
  ];

  programs._1password.enable = true;
  programs._1password-gui.enable = true;
}
