{
  description = "my nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      # home-manager should track the same nixpkgs as the system
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      ...
    }:
    let
      shared = [
        ./modules/1password.nix
        ./modules/agda.nix
        ./modules/audio.nix
        ./modules/backup
        ./modules/bluetooth.nix
        ./modules/cli.nix
        ./modules/desktop.nix
        ./modules/dev.nix
        ./modules/docker.nix
        ./modules/emacs
        ./modules/espanso
        ./modules/eudoxia.nix
        ./modules/fastfetch
        ./modules/firefox
        ./modules/font
        ./modules/gargoyle
        ./modules/git.nix
        ./modules/gtk.nix
        ./modules/guile
        ./modules/haskell.nix
        ./modules/heic
        ./modules/heroic.nix
        ./modules/locale.nix
        ./modules/mime.nix
        ./modules/network.nix
        ./modules/nixconf.nix
        ./modules/nushell
        ./modules/perf.nix
        ./modules/python.nix
        ./modules/rm-xdg-dirs.nix
        ./modules/rust.nix
        ./modules/scanner.nix
        ./modules/scripts
        ./modules/sddm.nix
        ./modules/shell.nix
        ./modules/ssh.nix
        ./modules/thunar
        ./modules/todoist.nix
        ./modules/tools
        ./modules/typst.nix
        ./modules/wallpaper
        ./modules/wayland
        ./modules/wayland/foot
        ./modules/wayland/fuzzel
        ./modules/wayland/sway
        ./modules/wayland/swaylock
        ./modules/wayland/waybar
        ./modules/x11
        ./modules/x11/alacritty
        ./modules/x11/awesome
        ./modules/x11/bspwm
        ./modules/x11/emote
        ./modules/x11/espanso
        ./modules/x11/polybar
        ./modules/x11/redshift
        ./modules/x11/spectrwm
        ./modules/x11/stumpwm
        ./modules/x11/xcape
        ./modules/x11/xcompose

        # Make home-manager use system pkgs
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
        }
      ];
    in
    {
      nixosConfigurations = {
        rostam = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = shared ++ [
            ./hosts/rostam/configuration.nix
            ./hosts/rostam/hardware-configuration.nix
            ./modules/beets
            ./modules/bright.nix
            ./modules/newsboat
            ./modules/epoch.nix
            ./modules/thetis.nix
            ./modules/zetanom
          ];
        };

        ismene = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = shared ++ [
            ./hosts/ismene/configuration.nix
            ./hosts/ismene/hardware-configuration.nix
          ];
        };
      };
    };
}
