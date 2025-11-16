{ pkgs, lib, ... }:

{
  # Custom Emacs package for Inform 7 mode
  # Source: https://github.com/GuiltyDolphin/inform7-mode
  # Package-Requires: ((emacs "27.1") (s "1.12.0"))
  inform7-mode = pkgs.emacsPackages.trivialBuild {
    pname = "inform7-mode";
    version = "0.1.1-unstable-2023-08-21";
    src = pkgs.fetchFromGitHub {
      owner = "GuiltyDolphin";
      repo = "inform7-mode";
      rev = "a409bbc6f04264f7f00616a995fa6ecf59d33d0d";
      sha256 = ""; # Run build to get hash, then replace with actual value
    };
    packageRequires = with pkgs.emacsPackages; [ s ];
  };

  # Custom Emacs package for XCompose mode
  # Source: https://github.com/kragen/xcompose
  xcompose-mode = pkgs.emacsPackages.trivialBuild {
    pname = "xcompose-mode";
    version = "unstable-2023-01-25";
    src = pkgs.fetchFromGitHub {
      owner = "kragen";
      repo = "xcompose";
      rev = "4d8eab4d05a19537ce79294ae0459fdae78ffb20";
      sha256 = ""; # Run build to get hash, then replace with actual value
    };
    packageRequires = [ ]; # No dependencies
  };

  # Custom Emacs package for Lean 4 mode
  # Source: https://github.com/leanprover-community/lean4-mode
  # Package-Requires: ((emacs "24.3") (dash "2.18.0") (lsp-mode "8.0.0") (magit-section "3.3.0"))
  lean4-mode = pkgs.emacsPackages.trivialBuild {
    pname = "lean4-mode";
    version = "unstable-2025-11-16";
    src = pkgs.fetchFromGitHub {
      owner = "leanprover-community";
      repo = "lean4-mode";
      rev = "1388f9d1429e38a39ab913c6daae55f6ce799479";
      sha256 = ""; # Run build to get hash, then replace with actual value
    };
    packageRequires = with pkgs.emacsPackages; [ dash lsp-mode magit-section ];
  };
}
