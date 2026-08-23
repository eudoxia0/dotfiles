{
  config,
  pkgs,
  lib,
  ...
}:

let
  cleanupXdgScript = pkgs.writeShellScriptBin "cleanup-xdg-dirs" ''
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
  # Disable automatic creation of XDG user directories.
  home-manager.users.eudoxia.xdg.userDirs = {
    enable = true;
    createDirectories = false;
  };

  # Install the cleanup script so it can be run manually.
  users.users.eudoxia.packages = [ cleanupXdgScript ];

  # Continuously remove unwanted XDG directories that applications recreate
  systemd.user.services.cleanup-xdg-dirs = {
    description = "Clean up unwanted XDG user directories";
    wantedBy = [ "default.target" ];

    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.writeShellScript "cleanup-xdg-dirs-loop" ''
        while true; do
          cd "$HOME" || exit 1
          for dir in Desktop Documents Downloads; do
            if [ -d "$dir" ]; then
              ${pkgs.coreutils}/bin/rmdir "$dir" 2>/dev/null || true
            fi
          done
          ${pkgs.coreutils}/bin/sleep 5
        done
      ''}";
      StandardOutput = "null";
      StandardError = "null";
    };
  };
}
