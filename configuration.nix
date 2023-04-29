{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./home.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # Enable swap on luks
  boot.initrd.luks.devices."luks-8731a1f4-b3ab-4db5-a669-be2bf61e8a2b".device = "/dev/disk/by-uuid/8731a1f4-b3ab-4db5-a669-be2bf61e8a2b";
  boot.initrd.luks.devices."luks-8731a1f4-b3ab-4db5-a669-be2bf61e8a2b".keyFile = "/crypto_keyfile.bin";

  networking.hostName = "sextant";

  # Enable networking
  networking.networkmanager.enable = true;

  networking.nameservers = [
    "8.8.8.8"
    "8.8.4.4"
  ];

  # Set your time zone.
  time.timeZone = "Australia/Sydney";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "intel" ];
  services.xserver.deviceSection = ''
    Option "DRI" "2"
    Option "TearFree" "true"
  '';
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.desktopManager.pantheon.enable = true;
  services.xserver.displayManager.defaultSession = "none+spectrwm";
  services.xserver.windowManager.spectrwm.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Passwordless sudo.
  security.sudo.extraRules = [
    {
      users = [ "eudoxia" ];
      commands = [
        {
          command = "ALL";
          options= [ "NOPASSWD" ];
        }
      ];
    }
  ];

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.eudoxia = {
    isNormalUser = true;
    description = "eudoxia";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
      # x11 apps
      firefox
      rxvt-unicode
      keepass
      calibre
      dmenu
      emacs
      vim
      pcmanfm
      spectrwm
      gnome.cheese
      gnome.gnome-sound-recorder
      font-manager
      dunst
      viewnior
      feh
      # x11
      xcape
      scrot
      xsecurelock
      # dev
      git
      podman
      gnuplot
      meld
      # utils
      whois
      htop
      curl
      acpi
      libnotify
      age
      neofetch
      # fonts
      terminus_font
      terminus_font_ttf
      inconsolata
    ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  fonts = {
    fontconfig.enable = true;
    fonts = with pkgs; [
      inconsolata
      terminus_font
      terminus_font_ttf
    ];
  };

  # System packages.
  environment.systemPackages = with pkgs; [];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  programs.ssh.startAgent = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [];
  networking.firewall.allowedUDPPorts = [];

  # Syncthing.
  services = {
    syncthing = {
      enable = true;
      user = "eudoxia";
      dataDir = "/home/eudoxia/files";
      configDir = "/home/eudoxia/.config/syncthing";
      guiAddress = "127.0.0.1:8384";
    };
  };

  # Disable Bluetooth.
  hardware.bluetooth.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
