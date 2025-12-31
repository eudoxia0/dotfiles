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
    curl
    fd
    gnumake
    graphviz
    imagemagick
    inform6
    inform7-nix.packages.${pkgs.stdenv.hostPlatform.system}.inform
    inform7-nix.packages.${pkgs.stdenv.hostPlatform.system}.inweb
    inform7-nix.packages.${pkgs.stdenv.hostPlatform.system}.intest
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
