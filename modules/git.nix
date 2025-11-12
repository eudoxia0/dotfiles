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

  home-manager.users.eudoxia.programs.git = {
    enable = true;
    lfs.enable = true;
    ignores = [
      ".DS_Store"
      "*~"
    ];
    settings = {
      user = {
        name = "Fernando Borretti";
        email = "fernando@borretti.me";
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
