{ config, pkgs, lib, ... }:

let
  customEmacs =
    ((pkgs.emacsPackagesFor pkgs.emacs-gtk).emacsWithPackages (epkgs: with epkgs; [
      olivetti
      slime
      github-theme
      sublime-themes
      aircon-theme
      company
      markdown-mode
      visual-regexp
      nix-mode
      rust-mode
      yaml-mode
      tuareg
      merlin
      hydra
      dockerfile-mode
      typescript-mode
      magit
      graphviz-dot-mode
      plantuml-mode
      treemacs
      lsp-mode
      projectile
    ]));
in
{
  imports = [
    <home-manager/nixos>
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
      "docker"
    ];
    packages = with pkgs; [
      # x11 apps
      firefox
      xterm
      keepassxc
      calibre
      dmenu
      customEmacs
      vscode
      leafpad
      vim
      mupdf
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
      chromium
      foliate
      dbeaver
      pavucontrol
      lxappearance
      liferea
      arandr
      djview
      baobab
      gnucash
      mpv
      gnome.dconf-editor
      pantheon.elementary-iconbrowser
      pdfslicer
      gscan2pdf
      # games
      endgame-singularity
      gargoyle
      # x11
      xcape
      scrot
      xsecurelock
      python3
      redshift
      xorg.xkill
      picom
      # dev
      git
      gnuplot
      meld
      jekyll
      gcc
      gdb
      valgrind
      clang
      jdk17
      gnumake
      sbcl
      gfortran
      fuse-overlayfs # docker perf
      rust-analyzer
      rustup
      clippy
      rustfmt
      cargo-tarpaulin
      # utils
      file
      whois
      htop
      curl
      acpi
      libnotify
      neofetch
      dig
      appimage-run
      zip
      unzip
      fd
      ripgrep
      graphviz
      pandoc
      texlive.combined.scheme-full
      killall
      gnupg
      restic
      # fonts
      terminus_font
      terminus_font_ttf
      inconsolata
      liberation_ttf
      doulos-sil
      gyre-fonts
      source-sans
    ];
  };

  #
  # Services
  #

  services.mullvad-vpn.enable = true;
  programs.dconf.enable = true;

  # Docker.
  virtualisation.docker = {
    enable = true;
    daemon.settings = {
      dns = [
        "8.8.8.8"
        "8.8.4.4"
      ];
    };
    rootless = {
      enable = true;
      setSocketVariable = true;
    };
  };

  # Podman.
  virtualisation = {
    podman = {
      enable = true;
      defaultNetwork.settings.dns_enabled = true;
    };
  };

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

  # GnuPG agent.
  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "curses";
  };

  # Firejail
  programs.firejail.enable = true;

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
      ".config/x11/autostart.sh" = {
        source = ./sources/autostart.sh;
      };
      ".garglkrc" = {
        source = ./sources/garglkrc.conf;
      };
      ".emacs.d/init.el" = {
        source = ./sources/init.el;
      };
      ".emacs.d/lisp/iy-go-to-char.el" = {
        source = ./sources/iy-go-to-char.el;
      };
      ".emacs.d/lisp/austral-mode.el" = {
        source = ./sources/austral-mode.el;
      };
      ".config/git/config" = {
        source = ./sources/gitconfig.conf;
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
      ".config/containers/registries.conf" = {
        source = ./sources/container-registries.conf;
      };
      ".config/liferea/feedlist.opml" = {
        source = ./sources/feedlist.opml;
      };
      ".local/bin" = {
        source = ./sources/scripts;
        recursive = true;
      };
      ".local/share/eudoxia" = {
        source = ./sources/share;
        recursive = true;
      };
      ".config/gtk-3.0/bookmarks" = {
        source = ./sources/bookmarks.txt;
      };
      ".config/plank/dock1/launchers" = {
        source = ./sources/plank;
      };
    };

    # Other apps try to configure mimeapps.list. This tells home-manager to
    # override its contents.
    xdg.configFile."mimeapps.list".force = true;

    xdg = {
      enable = true;
      mimeApps = {
        enable = true;
        defaultApplications = {
          "text/plain" = "emacs.desktop";
          "text/html" = "firefox.desktop";
          "text/markdown" = "emacs.desktop";

          "image/png" = "viewnior.desktop";
          "image/jpeg" = "viewnior.desktop";
          "image/gif" = "viewnior.desktop";
          "image/tiff" = "viewnior.desktop";

          "application/pdf" = "org.gnome.Evince.desktop";
          "application/vnd.oasis.opendocument.text" = "writer.desktop";
          "application/epub+zip" = "com.github.johnfactotum.Foliate.desktop";
          "application/x-mobipocket-ebook" = "com.github.johnfactotum.Foliate.desktop";
        };
      };
    };

    dconf.settings = {
      "net/launchpad/plank/docks/dock1" = {
        theme = "Transparent";
        position = "bottom";
        icon-size = 64;
        dock-items = [
          "firefox.dockitem"
          "chromium-browser.dockitem"
          "io.elementary.files.dockitem"
          "emacs.dockitem"
          "xterm.dockitem"
          "calibre-gui.dockitem"
          "org.keepassxc.KeePassXC.dockitem"
          "leafpad.dockitem"
          "io.elementary.switchboard.dockitem"
        ];
      };
      "io/elementary/desktop/wingpanel" = {
        use-transparency = true;
      };
      "io/elementary/screenshot" = {
        folder-dir = "/home/eudoxia/files/0 Inbox";
      };
      "org/gnome/gnome-screenshot" = {
        auto-save-directory = "file:///home/eudoxia/files/0%20Inbox";
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
    fontconfig = {
      enable = true;
      antialias = true;
      localConf = ''
       <dir>/home/eudoxia/files/4 Library/Fonts/in-use/</dir>
      '';
    };
    fonts = with pkgs; [
      inconsolata
      terminus_font
      terminus_font_ttf
      liberation_ttf
      doulos-sil
      gyre-fonts
      b612
      source-sans
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
  networking.resolvconf.enable = true;
  # Disable systemd-resolved
  services.resolved.enable = false;

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
  services.xserver.desktopManager.pantheon.extraSwitchboardPlugs = [ pkgs.pantheon-tweaks ];
  services.xserver.displayManager.defaultSession = "pantheon";
  services.xserver.windowManager.spectrwm.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  #
  # Security
  #

  programs.ssh.startAgent = true;

  networking.firewall.enable = true;
  # Syncthing ports
  networking.firewall.allowedTCPPorts = [ 8384 22000 ];
  networking.firewall.allowedUDPPorts = [ 22000 21027 ];
  # Docker networking
  networking.firewall.trustedInterfaces = [ "docker0" ];

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
