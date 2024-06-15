{
    description = "dotfiles";

    inputs = {
        nixpkgs = {
            url = "github:nixos/nixpkgs/nixpkgs-24.05-darwin";
        };
        nix-darwin = {
            url = "github:LnL7/nix-darwin/master";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        home-manager = {
            url = "github:nix-community/home-manager/release-24.05";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs = inputs @ { self, nixpkgs, nix-darwin, home-manager }: {
        darwinConfigurations = {
            metauro = nix-darwin.lib.darwinSystem {
                system = "aarch64-darwin";
                specialArgs = { inherit inputs; };
                modules = [
                    ./darwin.nix
                    home-manager.darwinModules.home-manager
                    {
                        home-manager.useGlobalPkgs = true;
                        home-manager.useUserPackages = true;
                        # home-manager.users.eudoxia = import ./home.nix;
                    }
                ];
            };
        };
    };
}
