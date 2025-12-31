{
  config,
  pkgs,
  lib,
  ...
}:

let
  cleanupXdgScript = pkgs.writeShellScript "cleanup-xdg-dirs" ''
    # Remove unwanted XDG user directories that applications recreate
    cd "$HOME" || {
      exit 1
    }

    for dir in Desktop Documents Downloads; do
      if [ -d "$dir" ]; then
        ${pkgs.coreutils}/bin/rmdir "$dir"
      fi
    done
  '';
in
{
  users.users.eudoxia = {
    isNormalUser = true;
    description = "eudoxia";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
  };

  # Disable automatic creation of XDG user directories.
  home-manager.users.eudoxia.xdg.userDirs = {
    enable = true;
    createDirectories = false;
  };

  # Periodically remove unwanted XDG directories that applications recreate
  home-manager.users.eudoxia.systemd.user.services.cleanup-xdg-dirs = {
    Unit = {
      Description = "Clean up unwanted XDG user directories";
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${cleanupXdgScript}";
    };
  };

  home-manager.users.eudoxia.systemd.user.timers.cleanup-xdg-dirs = {
    Unit = {
      Description = "Timer for cleaning up unwanted XDG user directories";
    };
    Timer = {
      OnBootSec = "5m";
      OnUnitActiveSec = "10m";
    };
    Install = {
      WantedBy = [ "timers.target" ];
    };
  };
}
