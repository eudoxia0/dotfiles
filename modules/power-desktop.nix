{
  config,
  pkgs,
  lib,
  ...
}:

{
  environment.systemPackages = with pkgs; [
    acpi
  ];

  powerManagement.enable = true;
  powerManagement.cpuFreqGovernor = "performance";
}
