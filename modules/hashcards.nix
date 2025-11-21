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
in
{
  home-manager.users.eudoxia = {
    home.packages = [
      hashcards
    ];
  };
}
