{
  config,
  pkgs,
  lib,
  ...
}:

let
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
in
{
  home-manager.users.eudoxia = {
    home.packages = [
      koboexport
    ];
  };
}
