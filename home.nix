{ config, pkgs, ... }:
let
  home-manager = builtins.fetchTarball {
    url = "https://github.com/nix-community/home-manager/archive/release-22.11.tar.gz";
    sha256 = "1cp2rpprcfl4mjsrsrpfg6278nf05a0mpl3m0snksvdalfmc5si5";
  };
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