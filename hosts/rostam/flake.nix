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
          ./rostam.nix
          ./mod/perf.nix
          ./mod/font.nix
          ./mod/ssh.nix
          ./mod/emacs
          ./mod/alacritty
          ./mod/git.nix
          ./mod/pkg.nix
          ./mod/redshift.nix

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
