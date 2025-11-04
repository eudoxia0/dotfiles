{ config, pkgs, ... }:

{
  # Keep AMD microcode up to date.
  hardware.cpu.amd.updateMicrocode = true;
}
