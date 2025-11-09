{
  config,
  pkgs,
  lib,
  ...
}:

{
  environment.systemPackages = with pkgs; [
    clang
    curl
    gnumake
    sqlite
    vim
  ];

  home-manager.users.eudoxia.home.packages = with pkgs; [
    agda
    btop
    fd
    imagemagick
    inform7
    just
    nixfmt-tree
    tokei
    tree
  ];
}
