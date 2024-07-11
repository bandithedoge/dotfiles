{...}: {
  nix = {
    enable = true;
    settings = {
      auto-optimise-store = true;
      max-call-depth = 100000;
      trusted-users = ["root" "@wheel" "bandithedoge"];
      use-xdg-base-directories = true;

      substituters = [
        "https://bandithedoge.cachix.org"
        "https://berberman.cachix.org"
        "https://cache.garnix.io"
        "https://cache.nixos.org"
        "https://emacsng.cachix.org"
        "https://ezkea.cachix.org"
        "https://hyprland.cachix.org"
        "https://jakestanger.cachix.org"
        "https://kira-bruneau.cachix.org"
        "https://nix-community.cachix.org"
        "https://nix-gaming.cachix.org"
        "https://nixpkgs-wayland.cachix.org"
      ];
      trusted-public-keys = [
        "bandithedoge.cachix.org-1:ZtcHw1anyEa4t6H8m3o/ctYFrwYFPAwoENSvofamE6g="
        "berberman.cachix.org-1:UHGhodNXVruGzWrwJ12B1grPK/6Qnrx2c3TjKueQPds="
        "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "emacsng.cachix.org-1:i7wOr4YpdRpWWtShI8bT6V7lOTnPeI7Ho6HaZegFWMI="
        "ezkea.cachix.org-1:ioBmUbJTZIKsHmWWXPe1FSFbeVe+afhfgqgTSNd34eI="
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "jakestanger.cachix.org-1:VWJE7AWNe5/KOEvCQRxoE8UsI2Xs2nHULJ7TEjYm7mM="
        "kira-bruneau.cachix.org-1:FJSccwNPRNHPBHN+qxAme2Svp537q7dDuHkqLnyOTaQ="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      ];
    };
    extraOptions = ''
      extra-experimental-features = nix-command flakes
      log-lines = 50
      warn-dirty = false
    '';
  };

  nixpkgs.config.allowUnfree = true;
}
