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
        ./modules/emote.nix
        # ./modules/espanso
        ./modules/eudoxia.nix
        ./modules/fastfetch
        ./modules/firefox
        ./modules/font
        ./modules/foot.nix
        ./modules/fuzzel.nix
        ./modules/gargoyle
        ./modules/gh.nix
        ./modules/git.nix
        ./modules/greetd.nix
        ./modules/gtk.nix
        ./modules/guile
        ./modules/haskell.nix
        ./modules/heic
        ./modules/heroic.nix
        ./modules/keyd.nix
        ./modules/locale.nix
        ./modules/mime.nix
        ./modules/network.nix
        ./modules/nixconf.nix
        ./modules/nushell
        ./modules/perf.nix
        ./modules/pulumi.nix
        ./modules/python.nix
        ./modules/redshift.nix
        ./modules/rm-xdg-dirs.nix
        ./modules/rust.nix
        ./modules/scanner.nix
        ./modules/scripts
        ./modules/shell.nix
        ./modules/ssh.nix
        ./modules/sway
        ./modules/thunar
        ./modules/todoist.nix
        ./modules/tools
        ./modules/typst.nix
        ./modules/wallpaper
        ./modules/waybar
        ./modules/x11
        ./modules/xscreensaver

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
