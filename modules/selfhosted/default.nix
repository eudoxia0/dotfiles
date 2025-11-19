{
  config,
  pkgs,
  lib,
  ...
}:

let
  katexTarball = pkgs.fetchurl {
    url = "https://github.com/KaTeX/KaTeX/releases/download/v0.16.25/katex.tar.gz";
    sha256 = "sha256-RUPIiaGi3s+iZ3NFjfZybEGRx+wwplre4l0Woojm1SU=";
  };

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
        sha256 = "sha256-uxGUMWSRAMaLow6X0MvHIEd1dz8+gw6D5lV5WnDkOs0=";
      };
      cargoHash = "sha256-c2RWugFHzgiVmVuwGlgOILJo/Y1QysLs9jU4GCWgxjc=";

      preBuild = ''
        mkdir -p vendor
        tar -xzf ${katexTarball} -C vendor
        # Rewrite font paths in CSS
        substituteInPlace vendor/katex/katex.min.css \
          --replace 'fonts/' '/katex/fonts/'
      '';

      nativeBuildInputs = [
        pkgs.pkg-config
        pkgs.gnutar
        pkgs.gzip
      ];

      buildInputs = [
        pkgs.openssl
      ];
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
