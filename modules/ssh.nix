{
  config,
  pkgs,
  lib,
  ...
}:

{
  programs.ssh.extraConfig = ''
    Host *
      AddKeysToAgent yes
  '';

  services.gnome.gnome-keyring.enable = true;
  services.gnome.gcr-ssh-agent.enable = true;

  environment.sessionVariables = {
    SSH_AUTH_SOCK = "/run/user/1000/gcr/ssh";
  };
}
