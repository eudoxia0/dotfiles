{
  config,
  pkgs,
  lib,
  ...
}:

{
  services.redshift.enable = true;
  services.redshift.temperature = {
    day = 6500;
    night = 2500;
  };
  location = {
    latitude = -33.8;
    longitude = 151.2;
  };
}
