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
    graphviz
    imagemagick
    inform6
    jekyll
    jq
    just
    lean4
    libxml2 # xmllint
    nixfmt-tree
    sass
    sqlite
    tokei
    tree
    vim
    zola
  ];
}
