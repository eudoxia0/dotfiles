{
  config,
  pkgs,
  lib,
  dotfilesDir,
  ...
}:

{
  programs.firefox.enable = true;

  programs.firefox.languagePacks = [ "en-US" ];

  # about:config
  programs.firefox.preferences = {
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
    "font.name.sans-serif.x-western" = "DejaVu Sans";
    "font.name.monospace.x-western" = "DejaVu Sans Mono";
    "font.size.variable.x-western" = 16;

    # Big-ass scrollbar.
    "widget.non-native-theme.scrollbar.style" = 4;
    "widget.non-native-theme.scrollbar.size.override" = 24;

    # Always show the scrollbar.
    "widget.gtk.overlay-scrollbars.enabled" = false;

    # Use smooth scrolling.
    "general.smoothScroll" = true;

    # Middle click to scroll
    "general.autoScroll" = true;
  };

  # about:policies
  programs.firefox.policies = {
    DisableFirefoxStudies = true;

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

    # Disable the Firefox password manager.
    PasswordManagerEnabled = false;

    # Do not prompt for a download location.
    PromptForDownloadLocation = false;

    # Disable search suggestions.
    SearchSuggestEnabled = false;

    # Save PDFs to disk instead of opening them.
    Handlers = {
      mimeTypes = {
        "application/pdf" = {
          action = "saveToDisk";
        };
      };
    };

    # Disable user messaging.
    UserMessaging = {
      # Don't recommend extensions.
      ExtensionRecommendations = false;
      # Don't recommend browser features.
      FeatureRecommendations = false;
      # Don't show "Firefox Labs" section in preferences.
      FirefoxLabs = false;
      # Allow me to change these preferences.
      Locked = false;
      # Don't show the “More from Mozilla” section in preferences
      MoreFromMozilla = false;
      # Skip new tab page onboarding.
      SkipOnboarding = true;
      # Don't offer Firefox-specific suggestions in the address bar.
      UrlbarInterventions = false;
    };
  };

  systemd.tmpfiles.rules = [
    "L+ /home/eudoxia/.config/mozilla/firefox/default/persdict.dat - - - - ${dotfilesDir}/modules/firefox/words.txt"
    "L+ /home/eudoxia/.config/mozilla/firefox/profiles.ini - - - - ${dotfilesDir}/modules/firefox/profiles.ini"
    "L+ /home/eudoxia/.config/mozilla/firefox/default/chrome/userContent.css - - - - ${dotfilesDir}/modules/firefox/userContent.css"
  ];
}
