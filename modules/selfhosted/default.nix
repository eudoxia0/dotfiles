{
  config,
  pkgs,
  lib,
  ...
}:

let
  customPackages = {
    koboexport = pkgs.rustPlatform.buildRustPackage {
      pname = "koboexport";
      version = "unstable";
      src = pkgs.fetchgit {
        url = "https://github.com/eudoxia0/koboexport.git";
        rev = "HEAD";
        sha256 = "sha256-Tt0fpqU3Rj0dtgr+XQwagm6K1auOvZ5hTFdJREno8/Y=";
      };
      cargoHash = "sha256-EI2h0jgNVtcp2ftxToHnEOHYJ3SoLhcLZkop+iBjTQY=";
    };

    hashcards = pkgs.rustPlatform.buildRustPackage {
      pname = "hashcards";
      version = "unstable";
      src = pkgs.fetchgit {
        url = "https://github.com/eudoxia0/hashcards.git";
        rev = "HEAD";
        sha256 = "sha256-WaduFfG05aZFjKogO8Q65Doj7RkPtILoYhJKBdNiLOM=";
      };
      cargoHash = "sha256-c2RWugFHzgiVmVuwGlgOILJo/Y1QysLs9jU4GCWgxjc=";
    };
  };
in
{
  home-manager.users.eudoxia = {
    home.file = {
      ".config/zetanom/config.toml".source = ./zetanom.toml;
    };

    home.packages = [
      customPackages.koboexport
      customPackages.hashcards
    ];

    systemd.user.services.zetanom = {
      Unit = {
        Description = "zetanom";
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = "/home/eudoxia/.eudoxia.d/bin/zetanom serve";
        Restart = "on-failure";
        RestartSec = "5s";
        Environment = [
          "RUST_LOG=info"
        ];
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
