{
  config,
  pkgs,
  lib,
  dotfilesDir,
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

  emacs-packages = with pkgs.emacsPackages; [
    consult
    cabal-mode
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
