{ pkgs, ... }:

{
    home = {
        username = "eudoxia";
        homeDirectory = "/Users/eudoxia";
        stateVersion = "24.05";

        packages = with pkgs; [
            coreutils
            neofetch
            curl
            python3
            rsync
            cargo
        ];

        file = {
            ".gitconfig".source = ./home/.gitconfig;
            ".gitignore".source = ./home/.global-gitignore;
            ".config/zed/settings.json".source = ./home/.config/zed/settings.json;
            ".emacs.d/init.el".source = ./home/.emacs.d/init.el;
            ".emacs.d/eudoxia/eudoxia-general.el".source = ./home/.emacs.d/eudoxia/eudoxia-general.el;
            ".emacs.d/eudoxia/eudoxia-lisp.el".source = ./home/.emacs.d/eudoxia/eudoxia-lisp.el;
            ".emacs.d/eudoxia/eudoxia-markdown.el".source = ./home/.emacs.d/eudoxia/eudoxia-markdown.el;
            ".emacs.d/eudoxia/eudoxia-mode-line.el".source = ./home/.emacs.d/eudoxia/eudoxia-mode-line.el;
            ".emacs.d/eudoxia/eudoxia-olivetti.el".source = ./home/.emacs.d/eudoxia/eudoxia-olivetti.el;
            ".emacs.d/eudoxia/eudoxia-package.el".source = ./home/.emacs.d/eudoxia/eudoxia-package.el;
            ".emacs.d/eudoxia/eudoxia-splash.el".source = ./home/.emacs.d/eudoxia/eudoxia-splash.el;
            ".emacs.d/eudoxia/eudoxia-ui.el".source = ./home/.emacs.d/eudoxia/eudoxia-ui.el;
            ".emacs.d/eudoxia/eudoxia-xml.el".source = ./home/.emacs.d/eudoxia/eudoxia-xml.el;
            ".emacs.d/eudoxia/eudoxia.el".source = ./home/.emacs.d/eudoxia/eudoxia.el;
            ".hushlogin".text = "";
            ".local/bin/backup.sh".source = ./home/.local/bin/backup.sh;
            ".local/bin/canonicalize-screenshots.py".source = ./home/.local/bin/canonicalize-screenshots.py;
            ".local/bin/docker-blow-away.sh".source = ./home/.local/bin/docker-blow-away.sh;
            ".local/bin/timestamp.py".source = ./home/.local/bin/timestamp.py;
        };
    };

    programs = {
        zsh = {
            enable = true;

            shellAliases = {
                ls   = "ls -1 --color";
                gs   = "git status";
                gits = "git status";
                gb   = "git branch";
                gco  = "git checkout";
                gcam = "git commit -a -m";
                gu   = "git push -u origin HEAD";
                gd   = "git pull origin";
                cf   = "cargo fmt";
                ck   = "cargo check";
            };
            sessionVariables = {
              PATH = "$HOME/.local/bin:$PATH";
            };
        };
        home-manager = {
            enable = true;
        };
    };
}
