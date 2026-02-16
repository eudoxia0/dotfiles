{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.keyd = {
    enable = true;
    keyboards.default = {
      ids = [ "*" ];
      settings = {
        main = {
          leftshift = "overload(shift, S-9)";
          rightshift = "overload(shift, S-0)";
        };
      };
    };
  };
}
