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
          agda2-mode
          i3wm-config-mode
          kaolin-themes
          magit
          markdown-mode
          moe-theme
          nano-theme
          nix-mode
          nushell-mode
          olivetti
          rust-mode
          sly
          sublime-themes
          treemacs
          unfill
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
