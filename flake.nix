{
  description = "dotfiles";

  inputs = {
    nixpkgs {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, nixpkgs, nix-darwin, home-manager }: let
    nixpkgsConfig = {
    config.allowUnfree = true;
    };
  in {
    darwinConfigurations = {
        metauro = nix-darwin.lib.darwinSystem {
            system = "aarch64-darwin";
            specialArgs = { inherit inputs; };
            modules = [
                ./darwin.nix
            ];
        };
    };
  };
}
