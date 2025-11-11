{
  config,
  pkgs,
  lib,
  ...
}:

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
