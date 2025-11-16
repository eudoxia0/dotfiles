{
  config,
  pkgs,
  lib,
  ...
}:

let
  customPackages = {
    inform7-mode = pkgs.emacsPackages.trivialBuild {
      pname = "inform7-mode";
      version = "unstable";
      src = pkgs.fetchFromGitHub {
        owner = "alexispurslane";
        repo = "inform7-mode";
        rev = "f99e534768c816ec038f34126f88d816c2f7d9ff";
        sha256 = "sha256-r9Zzd8Ro3p+Bae11bf1WIeVWkbmg17RKLDqG4UcFT1o=";
      };
      packageRequires = with pkgs.emacsPackages; [
        s
      ];
    };
  };
in
{
  home-manager.users.eudoxia = {
    programs.emacs = {
      enable = true;
      package = pkgs.emacs-gtk;
      extraPackages =
        epkgs: with epkgs; [
          ag
          agda2-mode
          i3wm-config-mode
          just-mode
          kaolin-themes
          lsp-mode
          lsp-ui
          magit
          markdown-mode
          moe-theme
          nano-theme
          nix-mode
          nushell-mode
          olivetti
          projectile
          rust-mode
          sly
          sublime-themes
          treemacs
          unfill
          vertico
          yaml-mode
          customPackages.inform7-mode
        ];
    };

    # Copy Emacs Lisp files.
    home.file.".emacs.d/init.el".source = ./init.el;
    home.file.".emacs.d/eudoxia" = {
      source = ./eudoxia;
      recursive = true;
    };
  };
}
