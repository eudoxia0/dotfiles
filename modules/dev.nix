{
  config,
  pkgs,
  lib,
  inform7-nix,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    btop
    clang
    claude-code
    codex
    curl
    fd
    ghostscript
    gnumake
    graphviz
    helix
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
    texliveSmall
    tokei
    tree
    vim
    zed-editor
    zola
  ];
}
