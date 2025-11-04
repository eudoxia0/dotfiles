{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    pulsemixer
    pavucontrol
  ];

  # Disable PulseAudio in favor of PipeWire
  services.pulseaudio.enable = false;

  # Enable realtime kit for low-latency audio
  security.rtkit.enable = true;

  # PipeWire configuration
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
}
