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
    ruby
    sqlite
    vim
  ];

  home-manager.users.eudoxia.home.packages = with pkgs; [
    agda
    btop
    fd
    just
    imagemagick
    inform7
    nixfmt-tree
    tokei
  ];
}
