{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

let
  emacs-packages = with pkgs.emacsPackages; [
    cabal-mode
    consult
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
    typst-ts-mode
    unfill
    vertico
    vue-mode
    web-mode
    yaml-mode
    zenburn-theme
    zerodark-theme
  ];

  emacs-custom = ((pkgs.emacsPackagesFor pkgs.emacs-gtk).emacsWithPackages (epkgs: emacs-packages));
in
{
  users.users.eudoxia.packages = [ emacs-custom ];

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.emacs.d/early-init.el - - - - ${dotfilesDir}/modules/emacs/early-init.el"
    "L+ /home/eudoxia/.emacs.d/init.el - - - - ${dotfilesDir}/modules/emacs/init.el"
  ];

  environment.sessionVariables = {
    EDITOR = "emacs";
  };
}
