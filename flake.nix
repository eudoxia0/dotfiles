{
  description = "my nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
  };

  outputs =
    {
      self,
      nixpkgs,
      ...
    }:
    let
      dotfilesDir = "/home/eudoxia/root/1-workspace/dotfiles";

      shared = [
        ./modules/apps.nix
        ./modules/audio.nix
        ./modules/beets
        ./modules/bluetooth.nix
        ./modules/emacs
        ./modules/eudoxia.nix
        ./modules/fastfetch
        ./modules/firefox
        ./modules/font
        ./modules/gargoyle
        ./modules/git
        ./modules/gtk
        ./modules/guile
        ./modules/locale.nix
        ./modules/mimeapps
        ./modules/network.nix
        ./modules/nixconf.nix
        ./modules/nushell
        ./modules/perf.nix
        ./modules/scanner.nix
        ./modules/scripts
        ./modules/sddm.nix
        ./modules/services.nix
        ./modules/shell.nix
        ./modules/ssh.nix
        ./modules/syncthing.nix
        ./modules/thunar
        ./modules/wallpaper
        ./modules/webcam.nix
        ./modules/x11
        ./modules/x11/alacritty
        ./modules/x11/bspwm
        ./modules/x11/emote
        ./modules/x11/espanso
        ./modules/x11/fvwm
        ./modules/x11/launcher
        ./modules/x11/polybar
        ./modules/x11/redshift
        ./modules/x11/stumpwm
        ./modules/x11/xcape
        ./modules/x11/xcompose
        ./modules/x11/xscreensaver
      ];

      sharedMiranda = [
        ./modules/apps.nix
        ./modules/audio.nix
        ./modules/beets
        # ./modules/bluetooth.nix
        ./modules/emacs
        ./modules/eudoxia.nix
        ./modules/fastfetch
        ./modules/firefox
        ./modules/font
        # ./modules/gargoyle
        ./modules/git
        ./modules/gtk
        # ./modules/guile
        ./modules/locale.nix
        # ./modules/mimeapps
        # ./modules/network.nix
        ./modules/nixconf.nix
        ./modules/nushell
        # ./modules/perf.nix
        # ./modules/scanner.nix
        ./modules/scripts
        ./modules/sddm.nix
        # ./modules/services.nix
        ./modules/shell.nix
        # ./modules/ssh.nix
        ./modules/syncthing.nix
        ./modules/thunar
        ./modules/wallpaper
        # ./modules/webcam.nix
        ./modules/x11
        ./modules/x11/alacritty
        # ./modules/x11/bspwm
        ./modules/x11/emote
        ./modules/x11/espanso
        # ./modules/x11/fvwm
        ./modules/x11/launcher
        ./modules/x11/polybar
        ./modules/x11/redshift
        ./modules/x11/stumpwm
        ./modules/x11/xcape
        ./modules/x11/xcompose
        ./modules/x11/xscreensaver
      ];
    in
    {
      nixosConfigurations = {
        rostam = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit dotfilesDir; };
          modules = shared ++ [
            ./hosts/rostam/configuration.nix
            ./hosts/rostam/hardware-configuration.nix
            ./modules/brightness-desktop.nix
          ];
        };

        ismene = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit dotfilesDir; };
          modules = shared ++ [
            ./hosts/ismene/configuration.nix
            ./hosts/ismene/hardware-configuration.nix
            ./modules/brightness-laptop.nix
          ];
        };

        miranda = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit dotfilesDir; };
          modules = sharedMiranda ++ [
            ./hosts/miranda/configuration.nix
            ./hosts/miranda/hardware-configuration.nix
            ./modules/brightness-laptop.nix
          ];
        };
      };
    };
}
