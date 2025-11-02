{ config, pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    gcr
  ];

  programs.ssh.startAgent = false;

  programs.ssh.extraConfig = ''
    Host *
      AddKeysToAgent yes
  '';
  services.gnome.gcr-ssh-agent.enable = true;

  services.gnome.gnome-keyring.enable = true;

  security.pam.services.login.enableGnomeKeyring = true;

  home-manager.users.eudoxia.home.sessionVariables = {
    SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gcr/ssh";
  };
}
