{
  config,
  pkgs,
  lib,
  ...
}:

{
  # Tell the CPU governor to keep the CPU at the highest frequency.
  powerManagement.cpuFreqGovernor = "performance";

  # Use a performance-oriented kernel.
  boot.kernelPackages = pkgs.linuxPackages_zen;

  # Zen kernel tweaks.
  boot.kernel.sysctl = {
    "vm.swappiness" = 10;
    "kernel.sched_autogroup_enabled" = 0;
  };

  # Enable TRIM for the SSD.
  services.fstrim.enable = true;

  # Better for development with lots of file watching
  boot.kernel.sysctl."fs.inotify.max_user_watches" = 524288;

  # Keep less documentation.
  documentation = {
    enable = true;
    man.enable = true;
    doc.enable = false;
    info.enable = false;
    nixos.enable = false;
  };

  # Take up less space.
  boot.loader.systemd-boot.configurationLimit = 10;

  # Use ccache for faster rebuilds.
  programs.ccache.enable = true;
}
