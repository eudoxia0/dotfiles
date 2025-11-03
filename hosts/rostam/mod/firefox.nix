{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.programs.firefox = {
    enable = true;

    profiles.default = {
      id = 0;
      name = "default";
      isDefault = true;

      # Firefox preferences (about:config settings)
      settings = {
        # Disable "match whole words" in find bar by default
        "findbar.entireword" = false;

        # Other useful preferences you might want to configure:
        # "browser.startup.homepage" = "about:blank";
        # "privacy.trackingprotection.enabled" = true;
        # "browser.disableResetPrompt" = true;
      };
    };
  };
}
