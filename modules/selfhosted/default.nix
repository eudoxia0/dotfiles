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
        rev = "5500ee5c08577c8eebded61389addabdcd900a56";
        sha256 = "sha256-Tt0fpqU3Rj0dtgr+XQwagm6K1auOvZ5hTFdJREno8/Y=";
      };
      cargoHash = "sha256-EI2h0jgNVtcp2ftxToHnEOHYJ3SoLhcLZkop+iBjTQY=";
    };

    hashcards = pkgs.rustPlatform.buildRustPackage {
      pname = "hashcards";
      version = "0.2.1";
      src = pkgs.fetchgit {
        url = "https://github.com/eudoxia0/hashcards.git";
        rev = "843fd70d6386dc154612524023e7026f449ab52f";
        sha256 = "sha256-GuhQ57D7cZvNojEYuDwxVS0kwvrZCTKpqoMLEv6d5iQ=";
      };
      cargoHash = "sha256-4I6XL/YPz3pneM1diy9cVzwjxQaOShRiFLzaJAM0M00=";

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

    xbrzscale = pkgs.stdenv.mkDerivation {
      pname = "xbrzscale";
      version = "unstable-2020-05-30";

      src = pkgs.fetchFromGitHub {
        owner = "atheros";
        repo = "xbrzscale";
        rev = "a2d8dce723e8fab548bf8460b46eeebfc64abca6";
        sha256 = "sha256-DQbwU7ZnaWqqoeUrfuRCkej/gVAWbSlEI62CJReLK2A=";
      };

      buildInputs = with pkgs; [ SDL2 SDL2_image ];
      nativeBuildInputs = with pkgs; [ pkg-config ];

      buildPhase = ''
        make
      '';

      installPhase = ''
        mkdir -p $out/bin
        cp xbrzscale $out/bin/
      '';
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
      customPackages.xbrzscale
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
