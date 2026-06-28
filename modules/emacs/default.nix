{
  config,
  pkgs,
  lib,
  ...
}:

let
  customPackages = {
    cabal-mode = pkgs.emacsPackages.trivialBuild {
      pname = "cabal-mode";
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
          # customPackages.xcompose-mode # TODO: why does this get stuck building forever?
          consult
          customPackages.cabal-mode
          # customPackages.eat # TODO: uncomment when codeberg comes back online
          customPackages.inform7-mode
          embark
          fvwm-mode
          graphviz-dot-mode
          i3wm-config-mode
          just-mode
          kaolin-themes
          lsp-mode
          lsp-ui
          magit
          marginalia
          markdown-mode
          moe-theme
          nano-theme
          nix-mode
          nushell-mode
          olivetti
          orderless
          projectile
          ripgrep
          rust-mode
          sly
          sublime-themes
          treemacs
          treesit-grammars.with-all-grammars
          typst-ts-mode
          unfill
          vertico
          web-mode
          yaml-mode
          zenburn-theme
          zerodark-theme
        ];
    };

    # Copy init.el.
    home.file.".emacs.d/init.el".source = ./init.el;
    home.file.".emacs.d/early-init.el".source = ./early-init.el;

    home.sessionVariables = {
      EDITOR = "emacs";
    };
  };
}
