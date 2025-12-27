{
  config,
  pkgs,
  lib,
  ...
}:

let
  cleanupXdgScript = pkgs.writeShellScript "cleanup-xdg-dirs" ''
    # Remove unwanted XDG user directories that applications recreate
    echo "Starting cleanup, HOME=$HOME, PWD=$PWD"

    cd "$HOME" || {
      echo "ERROR: Failed to cd to HOME"
      exit 1
    }

    for dir in Desktop Documents Downloads; do
      if [ -d "$dir" ]; then
        echo "Attempting to remove $dir"
        error_output=$(${pkgs.coreutils}/bin/rmdir "$dir" 2>&1)
        if [ $? -eq 0 ]; then
          echo "Successfully removed $dir"
        else
          echo "Failed to remove $dir: $error_output"
        fi
      else
        echo "$dir does not exist"
      fi
    done

    echo "Cleanup complete"
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
      OnUnitActiveSec = "1m";
    };
    Install = {
      WantedBy = [ "timers.target" ];
    };
  };
}
