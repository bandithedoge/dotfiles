{pkgs, ...}: {
  nix = {
    package = pkgs.lib.mkForce pkgs.nixUnstable;
    settings = {
      substituters = [
        "https://bandithedoge.cachix.org"
        "https://cache.iog.io"
        "https://cache.nixos.org"
        "https://hyprland.cachix.org"
        "https://kira-bruneau.cachix.org"
        "https://nix-community.cachix.org"
        "https://nix-gaming.cachix.org"
        "https://nixpkgs-wayland.cachix.org"
      ];
      trusted-public-keys = [
        "bandithedoge.cachix.org-1:ZtcHw1anyEa4t6H8m3o/ctYFrwYFPAwoENSvofamE6g="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "kira-bruneau.cachix.org-1:FJSccwNPRNHPBHN+qxAme2Svp537q7dDuHkqLnyOTaQ="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      ];
      trusted-users = ["root" "@wheel" "bandithedoge"];
    };
    extraOptions = ''
      extra-experimental-features = nix-command flakes
      allow-dirty = true
      auto-optimise-store = true
      warn-dirty = false
      log-lines = 50
    '';
  };
}
