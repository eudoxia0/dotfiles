{ config, pkgs, lib, ... }:


{
  imports = [ ./hw.nix ];

  system.stateVersion = "25.05"; # DO NOT CHANGE
  nixpkgs.config.allowUnfree = true;

  # basic config
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices."luks-5e2a0183-ed29-499d-8741-ea27e08caf28".device = "/dev/disk/by-uuid/5e2a0183-ed29-499d-8741-ea27e08caf28";

  networking.networkmanager.enable = true;
  networking.hostName = "rostam";
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

  # Nix settings.
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # users
  users.users.eudoxia = {
    isNormalUser = true;
    description = "eudoxia";
    extraGroups = [ "networkmanager" "wheel" "i2c" ];
  };

  # services
  services.printing.enable = true;
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
  services.displayManager.ly.enable = true;
  services.displayManager.ly.settings = {
    animation = "doom";
    bigclock = "en";
    brightness_down_key = "null";
    brightness_up_key = "null";
    clear_password = true;
    custom_sessions = "/etc/xdg/wayland-sessions/";
    default_input = "password";
    doom_fire_height = 8;
    doom_fire_spread = 4;
    hide_version_string = true;
  };
  services.xserver.enable = true;
  services.xserver.windowManager.stumpwm.enable = true;
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };
  services.xserver.dpi = 168;
  services.xscreensaver.enable = true;
  services.redshift.enable = true;
  location = {
    latitude = -33.8;
    longitude = 151.2;
  };

  # cagebreak
  environment.etc."xdg/wayland-sessions/cagebreak.desktop".text = ''
      [Desktop Entry]
      Name=cagebreak
      Comment=Launch cagebreak
      Exec=${pkgs.cagebreak}/bin/cagebreak -c /home/eudoxia/.cagebreak
      Type=Application
      Keywords=Wayland;Compositor;
  '';

  # Enable the firewall.
  networking.firewall.enable = true;

  # programs
  programs.firefox.enable = true;
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [
      mako # notification system developed by swaywm maintainer
      grim # screenshot functionality
      slurp # screenshot functionality
      wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
      waybar
      wmenu
    ];
  };
  programs._1password.enable = true;
  programs._1password-gui = {
    enable = true;
  };

  # packages
  environment.systemPackages = with pkgs; [
    cagebreak
    clang
    curl
    gnumake
    pciutils # lspci
    (polybar.override {
      pulseSupport = true;
    })
    pulsemixer
    ruby
    rustup
    sqlite
    stow
    usbutils # lsusb
    vim
    xorg.xev
    xscreensaver
  ];
  environment.localBinInPath = true;

  # home manager
  home-manager.users.eudoxia = {
    home = {
      stateVersion = "25.05"; # DO NOT CHANGE
      shellAliases = {
        cdt = "cd ~/dotfiles/hosts/rostam";
        cf = "cargo +nightly fmt";
        ck = "cargo check";
        cl= "cargo clippy --all-targets -- -D warnings";
        fd = "fd -HI";
        find = "echo 'use fd instead'";
        gb = "git branch";
        gcam = "git commit -a -m";
        gco = "git checkout";
        gd = "git pull origin";
        gs = "git status";
        gu = "git push -u origin HEAD";
        ls = "ls -1 --color";
      };

      packages = with pkgs; [
        agda
        age
        arandr
        btop
        calibre
        chromium
        ddcutil
        fastfetch
        fd
        feh
        flowtime
        foot
        gimp3
        guile
        ideogram
        imagemagick
        inform7
        just
        libreoffice
        mate.caja
        neofetch
        nwg-look
        pavucontrol
        pcmanfm
        rox-filer
        seahorse
        signal-desktop
        taskwarrior-tui
        taskwarrior3
        todoist-electron
        typst
        wdisplays
        xcape
        xfce.thunar
        zed-editor
      ];
    };

    gtk = {
      enable = true;
      theme = {
        name = "Arc";
        package = pkgs.arc-theme;
      };
      iconTheme = {
        name = "WhiteSur";
        package = pkgs.whitesur-icon-theme;
      };
    };

    programs.bash.enable = true;

    services.espanso = {
      enable = true;
    };
  };

  # Fix a bug where `todoist-electron` thinks the timezone is `undefined` for some reason. Instead we explicitly set `TZ`.
  nixpkgs.overlays = [(self: super: {
    todoist-electron = super.symlinkJoin {
      name = "todoist-electron";
      paths = [ super.todoist-electron ];
      buildInputs = [ super.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/todoist-electron \
          --set TZ "Australia/Sydney"
      '';
    };
  })];

  # AMD-specific.
  hardware.cpu.amd.updateMicrocode = true;

  # Ensure I have GPU drivers.
  hardware.graphics.enable = true;

  # Add i2c to control monitor brightness from the terminal. Needed by
  # ddcutil.
  boot.kernelModules = [ "i2c-dev" ];
}
