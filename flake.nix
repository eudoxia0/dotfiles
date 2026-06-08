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
        ./modules/locale.nix
        ./modules/mime.nix
        ./modules/network.nix
        ./modules/nixconf.nix
        ./modules/nushell
        ./modules/perf.nix
        ./modules/python.nix
        ./modules/rust.nix
        ./modules/scanner.nix
        ./modules/scripts
        ./modules/sddm.nix
        ./modules/services/antenor.nix
        ./modules/services/cleanup-xdg-dirs.nix
        ./modules/services/epoch.nix
        ./modules/services/metauro.nix
        ./modules/services/thetis.nix
        ./modules/services/wallpaperd
        ./modules/services/zetanom.nix
        ./modules/shell.nix
        ./modules/ssh.nix
        ./modules/syncthing.nix
        ./modules/thunar
        ./modules/todoist.nix
        ./modules/typst.nix
        ./modules/x11
        ./modules/x11/alacritty
        ./modules/x11/bspwm
        ./modules/x11/emote
        ./modules/x11/espanso
        ./modules/x11/fvwm
        ./modules/x11/polybar
        ./modules/x11/redshift
        ./modules/x11/rofi
        ./modules/x11/xcape
        ./modules/x11/xcompose
        ./modules/x11/xscreensaver

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
