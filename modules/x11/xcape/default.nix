{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.services.xcape = {
    enable = true;
    mapExpression = {
      Shift_L = "parenleft";
      ShiftQ_R = "parenright";
    };
  };
}
