{ config, pkgs, ... }:

{
    services.nix-daemon.enable = true;
    environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

    programs.bash = {
        enable = true;
        interactiveShellInit = "fish";
    };

    services.skhd = {
        enable = true;
        skhdConfig = ''
            play : mpc toggle
            previous : mpc prev
            next : mpc next
        '';
    };

    imports = [ <home-manager/nix-darwin> ];
    users.users.bandithedoge = {
        name = "bandithedoge";
        home = "/Users/bandithedoge";
    };
    home-manager.users.bandithedoge = import ~/.config/nixpkgs/home.nix;
}
