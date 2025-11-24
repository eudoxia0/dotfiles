{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    foot
  ];

  home-manager.users.eudoxia.home.file = {
    ".config/foot/foot.ini".source = ./foot.ini;
  };
}
