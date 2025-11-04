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
        ./modules/locale.nix
        ./modules/python.nix
        ./modules/rust.nix

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
            ./modules/alacritty
            ./modules/audio.nix
            ./modules/bright.nix
            ./modules/cagebreak
            ./modules/cli.nix
            ./modules/desktop.nix
            ./modules/dev.nix
            ./modules/emacs
            ./modules/espanso
            ./modules/firefox
            ./modules/font.nix
            ./modules/gh.nix
            ./modules/git.nix
            ./modules/gtk.nix
            ./modules/guile
            ./modules/ly
            ./modules/mime.nix
            ./modules/network.nix
            ./modules/perf.nix
            ./modules/polybar
            ./modules/redshift.nix
            ./modules/shell.nix
            ./modules/ssh.nix
            ./modules/stumpwm
            ./modules/sway
            ./modules/syncthing.nix
            ./modules/thunar
            ./modules/todoist.nix
            ./modules/typst.nix
            ./modules/wallpaper
            ./modules/x11.nix
            ./modules/xcape.nix
            ./modules/xscreensaver
            ./modules/zed
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
