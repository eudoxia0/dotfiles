{
  config,
  pkgs,
  lib,
  ...
}:

let
  cleanupXdgScript = pkgs.writeShellScript "cleanup-xdg-dirs" ''
    # Remove unwanted XDG user directories that applications recreate
    cd "$HOME" || exit 1

    rmdir Desktop 2>/dev/null || true
    rmdir Documents 2>/dev/null || true
    rmdir Downloads 2>/dev/null || true
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
