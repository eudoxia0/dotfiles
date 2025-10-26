{ config, pkgs, lib, ... }:

let
  home-manager = builtins.fetchTarball https://github.com/nix-community/home-manager/archive/release-25.05.tar.gz;
in
{
  imports =
    [ ./hardware-configuration.nix
      (import "${home-manager}/nixos")
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.initrd.luks.devices."luks-5e2a0183-ed29-499d-8741-ea27e08caf28".device = "/dev/disk/by-uuid/5e2a0183-ed29-499d-8741-ea27e08caf28";

  networking.networkmanager.enable = true;
  time.timeZone = "Australia/Sydney";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_AU.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_AU.UTF-8";
    LC_IDENTIFICATION = "en_AU.UTF-8";
    LC_MEASUREMENT = "en_AU.UTF-8";
    LC_MONETARY = "en_AU.UTF-8";
    LC_NAME = "en_AU.UTF-8";
    LC_NUMERIC = "en_AU.UTF-8";
    LC_PAPER = "en_AU.UTF-8";
    LC_TELEPHONE = "en_AU.UTF-8";
    LC_TIME = "en_AU.UTF-8";
  };

  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  services.printing.enable = true;

  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  users.users.eudoxia = {
    isNormalUser = true;
    description = "eudoxia";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
      #  thunderbird
    ];
  };

  programs.firefox.enable = true;

  system.stateVersion = "25.05"; # DO NOT CHANGE

  # CUSTOM
  networking.hostName = "rostam";
  nixpkgs.config.allowUnfree = true;
  services.displayManager.ly.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.windowManager.stumpwm.enable = true;
  environment.systemPackages = with pkgs; [
    alacritty
    arandr
    chromium
    cosmic-files
    curl
    emacs-pgtk
    foot
    ghostty
    git
    grim # screenshot functionality
    kitty
    lxrandr
    mako # notification system developed by swaywm maintainer
    mate.caja
    nemo
    pciutils # lspci
    pcmanfm
    rox-filer
    slurp # screenshot functionality
    spaceFM
    todoist-electron
    usbutils # lsusb
    vim
    wdisplays
    wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
    xfce.thunar
    xfe
    zed-editor
  ];
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };

  programs._1password.enable = true;
  programs._1password-gui = {
    enable = true;
    # Certain features, including CLI integration and system authentication support,
    # require enabling PolKit integration on some desktop environments (e.g. Plasma).
    polkitPolicyOwners = [ "eudoxia" ];
  };

  environment.sessionVariables = {
    GDK_SCALE = "";
    GDK_DPI_SCALE = "";
    NIXOS_OZONE_WL = "1";
  };


  services.xserver.enable = true;

  programs.ssh = {
    extraConfig = ''
          Host *
          IdentityAgent ~/.1password/agent.sock
        '';
  };

  # END CUSTOM

  # HOME MANAGER
  home-manager.users.eudoxia = { pkgs, ... }: {
    nixpkgs.config.allowUnfree = true;

    home = {
      stateVersion = "25.05"; # DO NOT CHANGE
      shellAliases = {
        cf = "cargo fmt";
        ck = "cargo check";
        gb = "git branch";
        gcam = "git commit -a -m";
        gco = "git checkout";
        gd = "git pull origin";
        gs = "git status";
        gu = "git push -u origin HEAD";
        ls = "ls -1 --color";
      };
      file = {
        ".stumpwmrc".source = ./stumpwmrc.lisp;
        ".config/sway/config".source = ./sway.txt;
      };
    };

    programs.bash.enable = true;

    programs.git = {
      enable = true;
      extraConfig = {
        user = {
          name = "Fernando Borretti";
          email = "fernando@borretti.me";
          signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINWGraQs2xjwtUVuOB/CtNJPjWKtbFpkh3EvANIR9Ld1";
        };
        color = {
          ui = "auto";
        };
        gpg = {
          format = "ssh";
        };
        "gpg \"ssh\"" = {
          program = "${lib.getExe' pkgs._1password-gui "op-ssh-sign"}";
        };
        commit = {
          gpgsign = true;
        };
        alias = {
          undo = "reset --soft HEAD~1";
          tree = "log --pretty='%Cgreen%h%Creset [%ai] %s %Cred<%Creset%an%Cred>' --decorate --graph";
        };
      };
    };
  };
}
