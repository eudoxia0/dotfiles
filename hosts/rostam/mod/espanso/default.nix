{ config, pkgs, lib, ... }:

{
  home-manager.users.eudoxia.services.espanso.enable = true;

  home-manager.users.eudoxia.home.file = {
    ".config/espanso/match/base.yml".source = ./espanso.yaml;
  };
}
