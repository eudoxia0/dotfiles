{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    (agda.withPackages (p: [
      p.standard-library
    ]))
  ];
}
