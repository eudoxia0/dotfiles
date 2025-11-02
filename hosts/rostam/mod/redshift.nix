{ config, pkgs, lib, ... }:

{
  services.redshift.enable = true;
  location = {
    latitude = -33.8;
    longitude = 151.2;
  };
}
