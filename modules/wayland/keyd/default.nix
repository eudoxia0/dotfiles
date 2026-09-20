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

  services.keyd.keyboards = {
    default = {
      ids = [
        "*"
      ];
      settings = {
        main = {
          "leftshift" = "overload(shift, S-9)";
          "rightshift" = "overload(shift, S-0)";
        };
      };
    };
  };
}
