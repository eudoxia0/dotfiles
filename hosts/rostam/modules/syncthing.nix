{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.services.syncthing = {
    enable = true;
    guiAddress = "127.0.0.1:8384";
    settings.devices = {
      rostam = {
        name = "rostam";
        id = "H6XRBBB-7WXJPGN-GVMFX3T-BANURMZ-B5CYCIZ-ZMVNFEB-HQWGIXE-3AZ5EQ4";
      };
      antigone = {
        name = "antigone";
        id = "SZYNWMU-QR6FOET-AZ6UYIV-S4J7YZN-KQIEA5Q-N3BRPBT-VQGIAOR-3MSZTA7";
      };
    };
    settings.folders = {
      "root" = {
        id = "root";
        label = "Root";
        path = "/home/eudoxia/Root";
        devices = [ "antigone" ];
      };
    };
  };
}
