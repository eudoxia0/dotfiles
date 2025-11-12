{
  config,
  pkgs,
  lib,
  ...
}:

let
  # Wrapper script for scrot that saves screenshots to organized directories
  scrot-screenshot = pkgs.writeShellScriptBin "scrot-screenshot" ''
    # Get current year
    YEAR=$(date +%Y)

    # Create screenshot directory structure
    SCREENSHOT_DIR="$HOME/Root/4 Library/Images/Screenshots/Desktop/$YEAR"
    mkdir -p "$SCREENSHOT_DIR"

    # Generate filename with timestamp
    FILENAME="$(date +%Y-%m-%d-%H%M%S)_scrot.png"

    # Call scrot with the full path and pass through any arguments
    ${pkgs.scrot}/bin/scrot "$@" "$SCREENSHOT_DIR/$FILENAME"
  '';
in
{
  services.xserver.windowManager.stumpwm.enable = true;

  home-manager.users.eudoxia.home.packages = with pkgs; [
    feh
    scrot-screenshot
  ];

  home-manager.users.eudoxia.home.file = {
    ".stumpwm.d/init.lisp".source = ./stumpwm.lisp;
    ".stumpwm.d/gaps.lisp".source = ./gaps.lisp;
  };
}
