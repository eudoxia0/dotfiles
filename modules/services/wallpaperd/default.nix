{
  config,
  pkgs,
  lib,
  ...
}:

let
  wallpaperd = pkgs.writers.writePython3Bin "wallpaperd" { } (builtins.readFile ./wallpaperd.py);
in
{
  # Copy the wallpapers directory.
  home-manager.users.eudoxia.home.file.".eudoxia.d/data/wallpaper" = {
    source = ./wallpapers;
    recursive = true;
  };

  # Install feh and wallpaperd.
  home-manager.users.eudoxia.home.packages = [
    pkgs.feh
    wallpaperd
  ];

  # wallpaperd service: cycles through wallpapers every 300s.
  systemd.user.services.wallpaperd = {
    description = "Wallpaper daemon";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    path = [ pkgs.feh ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${wallpaperd}/bin/wallpaperd";
      Restart = "on-failure";
    };
  };
}
