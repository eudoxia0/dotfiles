{ config, pkgs, lib, ... }:

let
  home-manager = builtins.fetchTarball {
    url = "https://github.com/nix-community/home-manager/archive/64020f453bdf3634bf88a6bbce7f3e56183c8b2b.tar.gz";
    sha256 = "10g1br51g2d2d80k2z41nzz45rkdwaikmf0gppp6mpria6102w24";
  };
in
{
  imports =
    [ ./hw.nix
      (import "${home-manager}/nixos")
    ];

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
  services.xscreensaver.enable = true;
  services.redshift.enable = true;
  location = {
    latitude = -33.8;
    longitude = 151.2;
  };
  services.gnome.gnome-keyring.enable = true;

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
    ];
  };
  programs._1password.enable = true;
  programs._1password-gui = {
    enable = true;
    # Certain features, including CLI integration and system authentication support,
    # require enabling PolKit integration on some desktop environments (e.g. Plasma).
    # polkitPolicyOwners = [ "eudoxia" ];
  };
  # programs.ssh = {
  #   extraConfig = ''
  #         Host *
  #         IdentityAgent ~/.1password/agent.sock
  #       '';
  # };
  programs.ssh.startAgent = true;
  services.gnome.gcr-ssh-agent.enable = false;

  security.pam.services.login.enableGnomeKeyring = true;

  # packages
  environment.systemPackages = with pkgs; [
    agda
    age
    alacritty
    arandr
    btop
    calibre
    chromium
    clang
    curl
    ddcutil
    fastfetch
    fd
    feh
    flowtime
    font-manager
    foot
    gimp3
    git
    gnome-font-viewer
    gnumake
    guile
    ideogram
    imagemagick
    inform7
    just
    libreoffice
    mate.caja
    meld
    neofetch
    pavucontrol
    pciutils # lspci
    pcmanfm
    (polybar.override {
      pulseSupport = true;
    })
    pulsemixer
    rox-filer
    ruby
    rustup
    seahorse
    signal-desktop
    sqlite
    stow
    taskwarrior-tui
    taskwarrior3
    todoist-electron
    gcr
    typst
    usbutils # lsusb
    vim
    wdisplays
    xcape
    xfce.thunar
    xorg.xev
    xscreensaver
    zed-editor
  ];
  environment.localBinInPath = true;

  # fonts
  fonts = {
    enableDefaultPackages = false;
    enableGhostscriptFonts = true;
    packages = with pkgs; [
      fira-code
      inconsolata
      iosevka
      dejavu_fonts
      jetbrains-mono
      noto-fonts
      # noto-fonts-emoji
      gyre-fonts
      liberation_ttf
      terminus_font
      terminus_font_ttf
    ];
    fontconfig = {
      defaultFonts = {
        emoji = ["Apple Color Emoji"];
        # emoji = ["Noto Color Emoji"];
      };
    };
  };

  # home manager
  home-manager.users.eudoxia = {
    home = {
      stateVersion = "25.05"; # DO NOT CHANGE
      shellAliases = {
        cdt = "cd ~/dotfiles/hosts/rostam";
        cf = "cargo +nightly fmt";
        ck = "cargo check";
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
      sessionVariables = {
        SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent";
      };
    };

    programs.bash.enable = true;

    programs.git = {
      enable = true;
      settings = {
        user = {
          name = "Fernando Borretti";
          email = "fernando@borretti.me";
          signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHUS90fzTDy7mfwQIpDBFg1zjBPsL0eWrEN/kgeSl6eW";
        };
        color = {
          ui = "auto";
        };
        diff = {
          tool = "meld";
        };
        difftool = {
          prompt = false;
        };
        "difftool \"meld\"" = {
          cmd = "meld \"$LOCAL\" \"$REMOTE\"";
        };
        "mergetool \"meld\"" = {
          prompt = false;
          cmd = "meld \"$LOCAL\" \"$BASE\" \"$REMOTE\" --output=\"$MERGED\"";
          trustExitCode = "true";
        };
        # gpg = {
        #   format = "ssh";
        # };
        # "gpg \"ssh\"" = {
        #   program = "${lib.getExe' pkgs._1password-gui "op-ssh-sign"}";
        # };
        # commit = {
        #   gpgsign = true;
        # };
        alias = {
          undo = "reset --soft HEAD~1";
          tree = "log --pretty='%Cgreen%h%Creset [%ai] %s %Cred<%Creset%an%Cred>' --decorate --graph";
        };
      };
    };

    programs.emacs = {
      enable = true;
      package = pkgs.emacs-gtk;
      extraPackages = epkgs: with epkgs; [
        magit
        markdown-mode
        nano-theme
        nix-mode
        olivetti
        rust-mode
        sly
        kaolin-themes
        moe-theme
        sublime-themes
        treemacs
        unfill
      ];
    };

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

  # performance
  powerManagement.cpuFreqGovernor = "performance";

  boot.kernelPackages = pkgs.linuxPackages_zen;

  # Zen kernel tweaks.
  boot.kernel.sysctl = {
    "vm.swappiness" = 10;
    "kernel.sched_autogroup_enabled" = 0;
  };

  # Enable TRIM for the SSD.
  services.fstrim.enable = true;

  # Speed up compilation.
  nix.settings = {
    max-jobs = "auto";  # use all cores
    cores = 16;         # thread count
  };

  # Better for development with lots of file watching
  boot.kernel.sysctl."fs.inotify.max_user_watches" = 524288;

  # AMD-specific.
  hardware.cpu.amd.updateMicrocode = true;

  # Ensure I have GPU drivers.
  hardware.graphics.enable = true;

  # Add i2c to control monitor brightness from the terminal. Needed by
  # ddcutil.
  boot.kernelModules = [ "i2c-dev" ];
}
