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
