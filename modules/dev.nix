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
    just
    lean4
    libxml2 # xmllint
    nixfmt-tree
    sqlite
    tokei
    tree
    vagrant
    vim
    zola
  ];

  virtualisation.virtualbox.host.enable = true;
  users.users.eudoxia.extraGroups = [ "vboxusers" ];
}
