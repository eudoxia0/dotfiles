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
    claude-code
    curl
    fd
    gnumake
    imagemagick
    jekyll
    just
    lean4
    inform6
    libxml2 # xmllint
    nixfmt-tree
    sqlite
    tokei
    tree
    vim
    zola
  ];
}
