{ config, pkgs, ... }:

{
  # Keep Intel microcode up to date.
  hardware.cpu.intel.updateMicrocode = true;
}
