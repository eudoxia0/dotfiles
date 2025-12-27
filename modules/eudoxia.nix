{
  config,
  pkgs,
  lib,
  ...
}:

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
      ExecStart = "${pkgs.bash}/bin/bash -c 'cd ~ && rmdir Desktop Documents Downloads 2>/dev/null || true'";
    };
  };

  home-manager.users.eudoxia.systemd.user.timers.cleanup-xdg-dirs = {
    Unit = {
      Description = "Timer for cleaning up unwanted XDG user directories";
    };
    Timer = {
      OnBootSec = "5m";
      OnUnitActiveSec = "5m";
    };
    Install = {
      WantedBy = [ "timers.target" ];
    };
  };
}
