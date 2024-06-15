{ pkgs, ... }: {}

{
  home.username = "eudoxia";
  home.homeDirectory = "/Users/eudoxia";
  home.stateVersion = "23.11";
  programs.home-manager.enable = true;
}

# { pkgs, ... }:

# {
#     # home.stateVersion = "24.05";

#     # home.packages = with pkgs; [
#     #     neofetch
#     # ];

#     # programs = {
#     #     zsh = {
#     #         enable = true;

#     #         shellAliases = {
#     #             ls = "ls --color";
#     #         };
#     #     };
#     #     home-manager {
#     #         enable = true;
#     #     };
#     # };
# }
