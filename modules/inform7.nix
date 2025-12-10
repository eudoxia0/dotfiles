{
  pkgs,
  config,
  inform7,
  ...
}:

{
  home-manager.users.eudoxia = {
    home.packages = [
      inform7.packages.${pkgs.stdenv.hostPlatform.system}.default
    ];
  };
}
