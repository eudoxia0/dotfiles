# The Bluecurve theme for GTK.
{ stdenvNoCC, fetchFromGitHub }:

stdenvNoCC.mkDerivation {
  pname = "bluecurve";
  version = "unstable-2026-06-21";

  src = fetchFromGitHub {
    owner = "neeeeow";
    repo = "Bluecurve";
    rev = "d6d83b56c2d3fa28fc49c4c0453c25e6f27b402f";
    hash = "sha256-oFLAeooLhu0OgfiBenUKPXNxwqtSsa0GjCHlI3UU9yM=";
  };

  dontBuild = true;
  dontConfigure = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/themes $out/share/icons
    cp -r themes/* $out/share/themes/
    cp -r icons/*  $out/share/icons/

    # Delete broken symlinks (upstream problem, the kcmpartitions.png file is
    # missing for 32x32 and 36x36 sizes).
    find $out/share/icons -xtype l -delete

    runHook postInstall
  '';
}
