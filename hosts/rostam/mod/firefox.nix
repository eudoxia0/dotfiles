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

        # Don't warn me before opening `about:config`.
        "browser.aboutConfig.showWarning" = false;
      };
    };
  };
}
