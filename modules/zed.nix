{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.custom.zed;
in
{
  options.custom.zed = {
    fontSize = lib.mkOption {
      type = lib.types.int;
      default = 15;
      description = "Font size for Zed editor (both UI and buffer)";
    };
  };

  config = {
    home-manager.users.eudoxia.home.packages = with pkgs; [
      zed-editor
    ];

    home-manager.users.eudoxia.home.file.".config/zed/settings.json".text = builtins.toJSON {
      ui_font_size = cfg.fontSize;
      buffer_font_size = cfg.fontSize;
      features = {
        edit_prediction_provider = "copilot";
      };
      theme = {
        mode = "system";
        light = "macOS Classic Light";
        dark = "Gruvbox Dark Hard";
      };
      terminal = {
        shell = {
          program = "nu";
        };
      };
      project_panel = {
        auto_fold_dirs = false;
      };
      buffer_font_family = ".ZedMono";
      ui_font_family = ".ZedMono";
    };
  };
}
