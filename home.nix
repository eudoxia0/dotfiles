{ config, pkgs, ... }:
let
  home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/master.tar.gz";
in
{
  imports = [
    (import "${home-manager}/nixos")
  ];

  home-manager.users.eudoxia = {
    home.stateVersion = "22.11";

    home.file = {
      ".bashrc" = {
        source = ./sources/bashrc.sh;
      };
      ".config/spectrwm/spectrwm.conf" = {
        source = ./sources/spectrwm.conf;
      };
      ".xprofile" = {
        source = ./sources/xprofile.sh;
      };
      ".garglkrc" = {
        source = ./sources/garglkrc.conf;
      };
      ".XCompose" = {
        source = ./sources/XCompose;
      };
      ".emacs.d/init.el" = {
        source = ./sources/init.el;
      };
      ".config/git/config" = {
        source = ./sources/gitconfig.conf;
      };
      ".xmodmap" = {
        source = ./sources/xmodmap;
      };
      ".Xresources" = {
        source = ./sources/xresources;
      };
      ".local/bin" = {
        source = ./sources/scripts;
        recursive = true;
      };
    };
  };
}