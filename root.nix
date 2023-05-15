{ config, pkgs, lib, ... }:

let
  home-manager = builtins.fetchTarball {
    url = "https://github.com/nix-community/home-manager/archive/release-22.11.tar.gz";
    sha256 = "1cp2rpprcfl4mjsrsrpfg6278nf05a0mpl3m0snksvdalfmc5si5";
  };
in
{
  imports = [
    (import "${home-manager}/nixos")
  ];

  #
  # General
  #

  system.stateVersion = "22.11";
  environment.systemPackages = with pkgs; [];
  nixpkgs.config.allowUnfree = true;

  #
  # Users
  #

  users.users.eudoxia = {
    isNormalUser = true;
    description = "eudoxia";
    extraGroups = [
      "networkmanager"
      "wheel"
      # "vboxusers" # VirtualBox
    ];
    packages = with pkgs; [
      # x11 apps
      firefox
      rxvt-unicode
      keepass
      calibre
      dmenu
      emacs
      vscode
      vim
      xfce.thunar
      xfce.xfconf # needed for thunar
      spectrwm
      gnome.cheese
      gnome.gnome-sound-recorder
      font-manager
      dunst
      viewnior
      feh
      gimp
      signal-desktop
      libreoffice
      gparted
      diffpdf
      # x11
      xcape
      scrot
      xsecurelock
      python3
      redshift
      # dev
      git
      podman
      gnuplot
      meld
      jekyll
      gcc
      clang
      ocaml
      jdk17
      gnumake
      opam
      sbcl
      gfortran
      # utils
      file
      whois
      htop
      curl
      acpi
      libnotify
      age
      neofetch
      dig
      appimage-run
      zip
      unzip
      v4l-utils
      fd
      aspell
      aspellDicts.en
      graphviz
      pandoc
      texlive.combined.scheme-full
      # fonts
      terminus_font
      terminus_font_ttf
      inconsolata
    ];
  };

  #
  # Services
  #

  # VirtualBox.
  #virtualisation.virtualbox.host.enable = true;
  #users.extraGroups.vboxusers.members = [ "eudoxia" ];
  #virtualisation.virtualbox.guest.enable = true;
  #virtualisation.virtualbox.guest.x11 = true;

  # Syncthing.
  services = {
    syncthing = {
      enable = true;
      user = "eudoxia";
      dataDir = "/home/eudoxia/files";
      configDir = "/home/eudoxia/.config/syncthing";
      guiAddress = "127.0.0.1:8384";

      overrideDevices = true;
      overrideFolders = true;
      devices = {
        "bullroarer" = { id = "OSZS5JQ-H3U262N-ELG4DDF-6BJVBPD-M2HN4OD-D2AEIIH-T7TN7R6-DLQHWA3"; };
        "sextant" = { id = "R5FLI7N-4HVR44U-3C3C6SB-FPHAOGV-CQ6RQZT-4J7KORO-2LWB4MR-7D4WZQ7"; };
      };
      folders = {
        "files" = {
          path = "/home/eudoxia/files";
          devices = [ "bullroarer" "sextant" ];
        };
      };
    };
  };

  # Other services.
  services.printing.enable = true;

  # Touchpad support.
  services.xserver.libinput.enable = true;

  # Thumbnail server.
  services.tumbler.enable = true;

  #
  # Home Manager
  #

  home-manager.users.eudoxia = {
    home.stateVersion = "22.11";

    home.file = {
      ".bashrc" = {
        source = ./sources/bashrc.sh;
      };
      ".config/spectrwm/spectrwm.conf" = {
        source = ./sources/spectrwm.conf;
      };
      ".config/spectrwm/autostart.sh" = {
        source = ./sources/autostart.sh;
      };
      ".garglkrc" = {
        source = ./sources/garglkrc.conf;
      };
      ".XCompose" = {
        source = ./sources/XCompose;
      };
      ".emacs.d/init.el" = {
        source = ./sources/init.el;
      };
      ".config/git/config" = {
        source = ./sources/gitconfig.conf;
      };
      ".xmodmap" = {
        source = ./sources/xmodmap;
      };
      ".Xresources" = {
        source = ./sources/xresources;
      };
      ".sbclrc" = {
        source = ./sources/sbclrc.lisp;
      };
      ".config/common-lisp/source-registry.conf.d/10-lisp.conf" = {
        source = ./sources/10-lisp.conf;
      };
      ".local/bin" = {
        source = ./sources/scripts;
        recursive = true;
      };
    };
  };

  #
  # Locale
  #

  time.timeZone = "Australia/Sydney";
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

  #
  # Fonts
  #

  fonts = {
    fontconfig.enable = true;
    fonts = with pkgs; [
      inconsolata
      terminus_font
      terminus_font_ttf
    ];
  };

  #
  # Networking
  #

  networking.networkmanager.enable = true;
  networking.nameservers = [
    "8.8.8.8"
    "8.8.4.4"
  ];

  # Tell NetworkManager not to write /etc/resolv.conf
  networking.networkmanager.dns = "none";
  # Disable resolvconf.
  networking.resolvconf.enable = false;
  # Disable systemd-resolved
  services.resolved.enable = false;
  # Overwrite /etc/resolv.conf
  environment.etc."resolv.conf".text = ''
    nameserver 8.8.8.8
    nameserver 8.8.4.4
  '';

  #
  # Sound
  #

  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  #
  # X11
  #

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.greeters.gtk.enable = false;
  services.xserver.displayManager.lightdm.greeters.pantheon.enable = true;
  services.xserver.desktopManager.pantheon.enable = true;
  services.xserver.displayManager.defaultSession = "none+spectrwm";
  services.xserver.windowManager.spectrwm.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  #
  # Security
  #

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

  hardware.bluetooth.enable = false;

  programs.ssh.startAgent = true;

  networking.firewall.enable = true;
  # Syncthing ports
  networking.firewall.allowedTCPPorts = [ 8384 22000 ];
  networking.firewall.allowedUDPPorts = [ 22000 21027 ];

  #
  # Boot
  #

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  #
  # Hacks
  #

  # A hack to enable jar files to run.
  system.activationScripts.ldso = lib.stringAfter [ "usrbinenv" ] ''
    mkdir -m 0755 -p /lib64
    ln -sfn ${pkgs.glibc.out}/lib64/ld-linux-x86-64.so.2 /lib64/ld-linux-x86-64.so.2.tmp
    mv -f /lib64/ld-linux-x86-64.so.2.tmp /lib64/ld-linux-x86-64.so.2 # atomically replace
  '';
}
