{ pkgs, inputs, ... }: {
  nix = {
    package = pkgs.nixFlakes;
    settings = {
      substituters = [
        "https://nix-community.cachix.org"
        "https://cache.nixos.org"
        "https://nixpkgs-wayland.cachix.org"
        "https://kira-bruneau.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
        "kira-bruneau.cachix.org-1:FJSccwNPRNHPBHN+qxAme2Svp537q7dDuHkqLnyOTaQ="
      ];
      trusted-users = [ "root" "@wheel" "bandithedoge" ];
    };
    extraOptions = ''
      extra-experimental-features = nix-command flakes
      allow-dirty = true
      auto-optimise-store = true
      keep-derivations = false
      min-free = ${toString (512 * 1024 * 1024)}
      substitute = true
      warn-dirty = false
      pure-eval = false
      sandbox = relaxed
      log-lines = 50
    '';
  };

  nixpkgs.config = {
    allowBroken = true;
    allowUnfree = true;
  };

  users.users."bandithedoge" = {
    name = "bandithedoge";
    shell = pkgs.fish;
  };
  environment.shells = [ pkgs.fish pkgs.bashInteractive ];

  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
    roboto
  ];

  programs.fish.enable = true;
}
