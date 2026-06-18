{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Load uinput kernel module and set permissions for espanso
  boot.kernelModules = [ "uinput" ];
  services.udev.extraRules = ''
    KERNEL=="uinput", GROUP="input", MODE="0660"
  '';

  home-manager.users.eudoxia = hm: {
    services.espanso.enable = true;

    home.file.".config/espanso/match/base.yml".source =
      hm.config.lib.file.mkOutOfStoreSymlink "${hm.config.home.homeDirectory}/root/1-workspace/dotfiles/modules/x11/espanso/espanso.yaml";
  };
}
