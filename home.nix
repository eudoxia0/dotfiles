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
      ".emacs.d/eudoxia/" = {
        source = ./home/.emacs.d/eudoxia;
        recursive = true;
      };
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
        rc   = "./recrank.sh";
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
