{ config, pkgs, lib, ... }:

{
  home-manager.users.eudoxia = {
    programs.emacs = {
      enable = true;
      package = pkgs.emacs-gtk;
      extraPackages = epkgs: with epkgs; [
        kaolin-themes
        magit
        markdown-mode
        moe-theme
        nano-theme
        nix-mode
        olivetti
        rust-mode
        sly
        sublime-themes
        treemacs
        unfill
      ];
    };

    # Copyt Emacs Lisp files.
    home.file.".emacs.d/init.el".source = ./init.el;
    home.file.".emacs.d/eudoxia/inform7.el".source = ./inform7.el;
  };
}
