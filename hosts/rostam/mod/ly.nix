{ config, pkgs, lib, ... }:

{
  services.displayManager.ly.enable = true;
  services.displayManager.ly.settings = {
    animation = "doom";
    bigclock = "en";
    brightness_down_key = "null";
    brightness_up_key = "null";
    clear_password = true;
    custom_sessions = "/etc/xdg/wayland-sessions/";
    default_input = "password";
    doom_fire_height = 8;
    doom_fire_spread = 4;
    hide_version_string = true;
  };
}
