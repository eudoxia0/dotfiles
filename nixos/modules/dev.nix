{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    agda
    btop
    clang
    curl
    fd
    gnumake
    imagemagick
    inform7
    just
    nixfmt-tree
    sqlite
    tokei
    tree
    vim
  ];
}
