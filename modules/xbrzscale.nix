{
  config,
  pkgs,
  lib,
  ...
}:

let
  xbrzscale = pkgs.stdenv.mkDerivation {
    pname = "xbrzscale";
    version = "unstable";

    src = pkgs.fetchFromGitHub {
      owner = "atheros";
      repo = "xbrzscale";
      rev = "a2d8dce723e8fab548bf8460b46eeebfc64abca6";
      sha256 = "sha256-DQbwU7ZnaWqqoeUrfuRCkej/gVAWbSlEI62CJReLK2A=";
    };

    buildInputs = with pkgs; [
      SDL2
      SDL2_image
    ];
    nativeBuildInputs = with pkgs; [ pkg-config ];

    buildPhase = ''
      make
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp xbrzscale $out/bin/
    '';
  };
in
{
  home-manager.users.eudoxia = {
    home.packages = [
      xbrzscale
    ];
  };
}
