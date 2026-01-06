{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    pulumi
    pulumiPackages.pulumi-python
  ];
}
