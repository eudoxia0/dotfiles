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
  # Install the cleanup script so it can be run manually.
  users.users.eudoxia.packages = [ cleanupXdgScript ];

  # Continuously remove unwanted XDG directories that applications recreate.
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

  # Self-hosted archive management app.
  systemd.user.services.antenor = {
    description = "antenor";
    after = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];

    serviceConfig = {
      ExecStart = "/home/eudoxia/.eudoxia.d/bin/antenor serve";
      Restart = "on-failure";
      RestartSec = "5s";
      Environment = [
        "RUST_LOG=info"
        "ANTENOR_PORT=12004"
        "ANTENOR_DATA_DIRECTORY=/home/eudoxia/root/6-databases/antenor"
      ];
    };
  };

  # Self-hosted journal app.
  systemd.user.services.epoch = {
    description = "epoch";
    after = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];

    serviceConfig = {
      ExecStart = "/home/eudoxia/.eudoxia.d/bin/epoch serve";
      Restart = "on-failure";
      RestartSec = "5s";
      Environment = [
        "RUST_LOG=info"
        "EPOCH_PORT=12003"
        "EPOCH_DB_PATH=/home/eudoxia/root/6-databases/epoch/epoch.db"
      ];
    };
  };

  # Self-hosted note-taking app.
  systemd.user.services.metauro = {
    description = "metauro";
    after = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    wantedBy = [ "graphical-session.target" ];

    serviceConfig = {
      ExecStart = "/home/eudoxia/.eudoxia.d/bin/metauro serve";
      Restart = "on-failure";
      RestartSec = "5s";
      Environment = [
        "RUST_LOG=info"
        "METAURO_PORT=12005"
        "METAURO_DATA_DIRECTORY=/home/eudoxia/root/6-databases/metauro"
      ];
    };
  };
}
