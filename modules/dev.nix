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
    inform7
    jekyll
    just
    lean4
    libxml2 # xmllint
    nixfmt-tree
    sqlite
    tokei
    tree
    vim
    zola
  ];
}
