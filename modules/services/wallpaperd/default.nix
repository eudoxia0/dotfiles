{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Copy the wallpapers directory.
  home-manager.users.eudoxia.home.file.".eudoxia.d/data/wallpaper" = {
    source = ./wallpapers;
    recursive = true;
  };

  # Install feh.
  home-manager.users.eudoxia.home.packages = with pkgs; [
    feh
  ];

  # wallpaperd service: cycles through wallpapers every 300s.
  systemd.user.services.wallpaperd = {
    description = "Wallpaper daemon";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = let
        wallpaperDir = "%h/.eudoxia.d/data/wallpaper";
        script = pkgs.writeShellScript "wallpaperd" ''
          WALLPAPER_DIR="${wallpaperDir}"
          while true; do
            WALLPAPER=$(${pkgs.findutils}/bin/find "$WALLPAPER_DIR" -type f \( -name '*.jpg' -o -name '*.png' \) | ${pkgs.coreutils}/bin/shuf -n 1)
            if [ -n "$WALLPAPER" ]; then
              ${pkgs.feh}/bin/feh --no-fehbg --bg-fill "$WALLPAPER"
            fi
            sleep 300
          done
        '';
      in "${script}";
      Restart = "on-failure";
    };
  };
}
