{
  config,
  pkgs,
  lib,
  ...
}:

{
  home-manager.users.eudoxia.home.packages = with pkgs; [
    todoist-electron
  ];

  nixpkgs.overlays = [
    (self: super: {
      # This fixes a bug where `todoist-electron` thinks the timezone is
      # `undefined` for some reason. Instead we explicitly set `TZ`.
      todoist-electron = super.symlinkJoin {
        name = "todoist-electron";
        paths = [ super.todoist-electron ];
        buildInputs = [ super.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/todoist-electron \
            --set TZ "Australia/Sydney" \
        '';
      };
    })
  ];
}
