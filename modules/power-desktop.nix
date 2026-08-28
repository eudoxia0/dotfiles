{
  config,
  pkgs,
  lib,
  ...
}:

{
  environment.systemPackages = [ pkgs.acpi ];

  powerManagement.enable = true;
  powerManagement.cpuFreqGovernor = "performance";
}
