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

      # Disable Generative AI features.
      GenerativeAI = {
        Enabled = false;
        Chatbot = false;
        LinkPreviews = false;
        TabGroups = false;
        Locked = true;
      };

      # Disable autofill.
      AutofillAddressEnabled = false;
      AutofillCreditCardEnabled = false;

      # Only allow HTTPS.
      HttpsOnlyMode = "force_enabled";

      # Disable DNS prefetching.
      NetworkPrediction = false;

      # Disable the new tab page.
      NewTabPage = false;

      # Do not offer to save logins.
      OfferToSaveLogins = false;

      # Do not prompt for a download location.
      PromptForDownloadLocation = false;

      # Set the default download directory.
      # DownloadDirectory = "/home/eudoxia/Root/0 Inbox";

      # Disable search suggestions.
      SearchSuggestEnabled = false;

      # Install extensions.
      ExtensionSettings = {
        # uBlock Origin
        "uBlock0@raymondhill.net" = {
          "installation_mode" = "normal_installed";
          "install_url" =
            "https://addons.mozilla.org/firefox/downloads/latest/uBlock0@raymondhill.net/latest.xpi";
        };
        # 1Password
        "{d634138d-c276-4fc8-924b-40a0ea21d284}" = {
          "installation_mode" = "normal_installed";
          "install_url" =
            "https://addons.mozilla.org/firefox/downloads/latest/1password-x-password-manager/latest.xpi";
        };
        # Double-Click Image Downloader: https://github.com/eudoxia0/double-click-image-downloader
        "{5be538ce-6708-4b66-b3ce-81a25d2cbb0d}" = {
          "installation_mode" = "normal_installed";
          "install_url" =
            "https://github.com/eudoxia0/double-click-image-downloader/releases/download/v1.0/double-click-image-downloader.xpi";
        };
      };
    };

    profiles.default = {
      id = 0;
      name = "eudoxia";
      isDefault = true;
      userContent = ./userContent.css;

      # managed bookmarks
      bookmarks = {
        force = true;
        settings = [
          {
            name = "toolbar";
            toolbar = true;
            bookmarks = builtins.fromJSON (builtins.readFile ./bookmarks.json);
          }
        ];
      };

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

        # Enable userContent.css.
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true; # Don't show the top sites (Facebook etc.) in the new tab page.

        # Don't show the top sites (Facebook etc.) in the new tab page.
        "browser.newtabpage.activity-stream.feeds.topsites" = false;

        # Configure fonts.
        "font.name.serif.x-western" = "TeX Gyre Termes";
        "font.name.sans-serif.x-western" = "TeX Gyre Heros";
        "font.name.monospace.x-western" = "Fira Code";
        "font.size.variable.x-western" = 16;
      };
    };
  };

  home-manager.users.eudoxia.home.file.".mozilla/firefox/default/persdict.dat".source = ./words.txt;
}
