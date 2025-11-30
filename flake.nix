{
  description = "my nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
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
        ./modules/alacritty
        ./modules/audio.nix
        ./modules/backup
        ./modules/cli.nix
        ./modules/desktop.nix
        ./modules/dev.nix
        ./modules/docker.nix
        ./modules/emacs
        ./modules/emote
        ./modules/espanso
        ./modules/eudoxia.nix
        ./modules/fastfetch
        ./modules/firefox
        ./modules/font
        ./modules/gargoyle
        ./modules/gh.nix
        ./modules/git.nix
        ./modules/gtk.nix
        ./modules/guile
        ./modules/haskell.nix
        ./modules/heic
        ./modules/locale.nix
        ./modules/mime.nix
        ./modules/network.nix
        ./modules/newsboat
        ./modules/nixconf.nix
        ./modules/nushell
        ./modules/perf.nix
        ./modules/polybar
        ./modules/python.nix
        ./modules/redshift.nix
        ./modules/rust.nix
        ./modules/shell.nix
        ./modules/ssh.nix
        ./modules/stumpwm
        ./modules/syncthing.nix
        ./modules/thunar
        ./modules/todoist.nix
        ./modules/tools
        ./modules/typst.nix
        ./modules/wallpaper
        ./modules/x11
        ./modules/xbrzscale.nix
        ./modules/xcape.nix
        ./modules/xscreensaver
        ./modules/zed.nix
        ./modules/zetanom

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
            ./modules/bright.nix
            ./modules/ly
          ];
        };

        ismene = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = shared ++ [
            ./hosts/ismene/configuration.nix
            ./hosts/ismene/hardware-configuration.nix
            ./modules/ly
          ];
        };
      };
    };
}
