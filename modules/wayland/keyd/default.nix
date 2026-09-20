{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.keyd.enable = true;

  users.users.eudoxia.packages = with pkgs; [
    keyd
  ];
}
