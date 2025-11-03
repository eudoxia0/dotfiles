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
    {
      nixosConfigurations = {
        rostam = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./hw.nix
            ./mod/alacritty
            ./mod/audio.nix
            ./mod/bright.nix
            ./mod/cagebreak
            ./mod/cli.nix
            ./mod/desktop.nix
            ./mod/dev.nix
            ./mod/emacs
            ./mod/espanso
            ./mod/firefox
            ./mod/font.nix
            ./mod/gh.nix
            ./mod/git.nix
            ./mod/gtk.nix
            ./mod/locale.nix
            ./mod/ly
            ./mod/network.nix
            ./mod/perf.nix
            ./mod/polybar
            ./mod/python.nix
            ./mod/redshift.nix
            ./mod/rust.nix
            ./mod/shell.nix
            ./mod/ssh.nix
            ./mod/stumpwm
            ./mod/sway
            ./mod/todoist.nix
            ./mod/typst.nix
            ./mod/wallpaper
            ./mod/x11.nix
            ./mod/xcape.nix
            ./mod/xscreensaver
            ./mod/zed
            ./rostam.nix

            # Make home-manager use system pkgs
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
            }
          ];
        };
      };
    };
}
