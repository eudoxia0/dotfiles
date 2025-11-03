{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    meld
  ];

  home-manager.users.eudoxia.home.shellAliases = {
    gb = "git branch";
    gcam = "git commit -a -m";
    gco = "git checkout";
    gd = "git pull origin";
    gs = "git status";
    gu = "git push -u origin HEAD";
  };

  home-manager.users.eudoxia.programs.git = {
    enable = true;
    settings = {
      user = {
        name = "Fernando Borretti";
        email = "fernando@borretti.me";
        signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIELrqPWqe1qDaTvYXyM3fw0+ToGRN6R+1qqt3QN5vWCR fernando@borretti.me";
      };
      color = {
        ui = "auto";
      };
      diff = {
        tool = "meld";
      };
      difftool = {
        prompt = false;
      };
      "difftool \"meld\"" = {
        cmd = "meld \"$LOCAL\" \"$REMOTE\"";
      };
      "mergetool \"meld\"" = {
        prompt = false;
        cmd = "meld \"$LOCAL\" \"$BASE\" \"$REMOTE\" --output=\"$MERGED\"";
        trustExitCode = "true";
      };
      gpg = {
        format = "ssh";
      };
      commit = {
        gpgsign = true;
      };
      alias = {
        undo = "reset --soft HEAD~1";
        tree = "log --pretty='%Cgreen%h%Creset [%ai] %s %Cred<%Creset%an%Cred>' --decorate --graph";
      };
    };
  };
}
