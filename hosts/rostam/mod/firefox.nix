{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.programs.firefox = {
    enable = true;
    languagePacks = [ "en-US" ];

    # about:policies
    policies = {
      # Disable Firefox Sync.
      DisableFirefoxAccounts = true;

      # Disable the option to "Set as Desktop Background" when right-clicking
      # on an image.
      DisableSetDesktopBackground = true;

      # Turn off telemetry.
      DisableTelemetry = true;

      # Disable checking whether Firefox is the default browser.
      DontCheckDefaultBrowser = true;

      # Disable Generative AI features..
      GenerativeAI = {
        Enabled = false;
        Chatbot = false;
        LinkPreviews = false;
        TabGroups = false;
        Locked = false;
      };

      # Only allow HTTPS.
      HttpsOnlyMode = "force_enabled";
    };

    profiles.default = {
      id = 0;
      name = "eudoxia";
      isDefault = true;

      # about:config
      settings = {
        # Disable "match whole words" in find bar.
        "findbar.entireword" = false;

        # Highlight all search results.
        "findbar.highlightAll" = true;

        # Always show the bookmark bar.
        "browser.toolbars.bookmarks.visibility" = "always";

        # Don't warn me before opening `about:config`.
        "browser.aboutConfig.showWarning" = false;

        # Don't show the welcome page.
        "trailhead.firstrun.didSeeAboutWelcome" = true;
      };
    };
  };
}
