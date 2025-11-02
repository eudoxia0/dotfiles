{ config, pkgs, lib, ... }:

{

  home-manager.users.eudoxia.home.file.".local/share/panther.jpg".source = ./panther.jpg;
}
