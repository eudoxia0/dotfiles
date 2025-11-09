{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    btop
    clang
    curl
    fd
    gnumake
    imagemagick
    inform7
    jekyll
    just
    lean4
    nixfmt-tree
    sqlite
    tokei
    tree
    vim
    zola
  ];
}
