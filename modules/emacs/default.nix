{
  config,
  pkgs,
  lib,
  ...
}:

let
  customPackages = {
    cabal-mode = pkgs.emacsPackages.trivialBuild {
      pname = "eat";
      version = "unstable";
      src = pkgs.fetchFromGitHub {
        owner = "webdevred";
        repo = "cabal-mode";
        rev = "083a777e09bdb5a8d8d69862d44f13078664091f";
        sha256 = "sha256-c5dUsnEx+0uXFzxQLMnhiP8Gvwedzvq0F0BA+beBkmI=";
      };
      packageRequires = [ ];
    };

    eat = pkgs.emacsPackages.trivialBuild {
      pname = "eat";
      version = "unstable";
      src = pkgs.fetchgit {
        url = "https://codeberg.org/akib/emacs-eat.git";
        rev = "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99";
        sha256 = "sha256-9xG2rMlaMFY77JzUQ3JFrc7XKILZSL8TbP/BkzvBvMk=";
      };
      packageRequires = with pkgs.emacsPackages; [
        compat
      ];
    };

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

    lean4-mode = pkgs.emacsPackages.melpaBuild {
      pname = "lean4-mode";
      version = "1.1.2";
      src = pkgs.fetchFromGitHub {
        owner = "leanprover-community";
        repo = "lean4-mode";
        rev = "1388f9d1429e38a39ab913c6daae55f6ce799479";
        sha256 = "sha256-6XFcyqSTx1CwNWqQvIc25cuQMwh3YXnbgr5cDiOCxBk=";
      };
      packageRequires = with pkgs.emacsPackages; [
        dash
        lsp-mode
        magit-section
      ];
      files = ''("*.el" "data")'';
    };

    xcompose-mode = pkgs.emacsPackages.trivialBuild {
      pname = "xcompose-mode";
      version = "unstable";
      src = pkgs.fetchgit {
        url = "git://git.thomasvoss.com/xcompose-mode.git";
        rev = "aeb03f9144e39c882ca6c5c61b9ed1300a2a12ee";
        sha256 = "sha256-lPapwSJKG+noINmT1G5jNyUZs5VykMOSKJIbQxBWLEA=";
      };
      packageRequires = [ ];
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
          customPackages.cabal-mode
          customPackages.eat
          customPackages.inform7-mode
          customPackages.lean4-mode
          customPackages.xcompose-mode
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
        ];
    };

    # Copy init.el.
    home.file.".emacs.d/init.el".source = ./init.el;
  };
}
