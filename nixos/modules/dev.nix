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
    imagemagick
    inform7
    just
    nixfmt-tree
    tokei
    tree
  ];

  home-manager.users.eudoxia = {
    programs.bash.enable = true;
    home = {
      shellAliases = {
        fd = "fd -HI";
        find = "echo 'use fd instead'";
      };
    };
  };
}
