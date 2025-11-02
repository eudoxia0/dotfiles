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

  outputs = { self, nixpkgs, home-manager, ... }: {
    nixosConfigurations = {
      rostam = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./mod/alacritty
          ./mod/audio.nix
          ./mod/emacs
          ./mod/font.nix
          ./mod/git.nix
          ./mod/gtk.nix
          ./mod/locale.nix
          ./mod/ly.nix
          ./mod/network.nix
          ./mod/perf.nix
          ./mod/pkg.nix
          ./mod/redshift.nix
          ./mod/shell.nix
          ./mod/ssh.nix
          ./mod/sway
          ./mod/espanso
          ./mod/stumpwm
          ./mod/polybar
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
