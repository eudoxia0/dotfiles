{
  config,
  pkgs,
  lib,
  ...
}:

let
  # Import custom Emacs packages
  customEmacsPackages = import ./packages.nix { inherit pkgs lib; };
in
{
  home-manager.users.eudoxia = {
    programs.emacs = {
      enable = true;
      package = pkgs.emacs-gtk;
      extraPackages =
        epkgs: with epkgs; [
          # Standard packages from nixpkgs
          ag
          agda2-mode
          dash
          i3wm-config-mode
          just-mode
          kaolin-themes
          lsp-mode
          lsp-ui
          magit
          magit-section
          markdown-mode
          moe-theme
          nano-theme
          nix-mode
          nushell-mode
          olivetti
          projectile
          rust-mode
          s
          sly
          sublime-themes
          treemacs
          unfill
          vertico
          yaml-mode

          # Custom packages
          customEmacsPackages.inform7-mode
          customEmacsPackages.lean4-mode
          customEmacsPackages.xcompose-mode
        ];
    };

    # Copy Emacs Lisp files.
    home.file.".emacs.d/init.el".source = ./init.el;
  };
}
