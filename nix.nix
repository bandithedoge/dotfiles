{pkgs, ...}: {
  nix = {
    package = pkgs.nixVersions.stable;
    settings = {
      substituters = [
        "https://bandithedoge.cachix.org"
        "https://cache.nixos.org"
        "https://kira-bruneau.cachix.org"
        "https://nix-community.cachix.org"
        "https://nixpkgs-wayland.cachix.org"
        "https://cache.iog.io"
        "https://nix-gaming.cachix.org"
      ];
      trusted-public-keys = [
        "bandithedoge.cachix.org-1:ZtcHw1anyEa4t6H8m3o/ctYFrwYFPAwoENSvofamE6g="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "kira-bruneau.cachix.org-1:FJSccwNPRNHPBHN+qxAme2Svp537q7dDuHkqLnyOTaQ="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
        "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
      ];
      trusted-users = ["root" "@wheel" "bandithedoge"];
    };
    extraOptions = ''
      extra-experimental-features = nix-command flakes
      allow-dirty = true
      auto-optimise-store = true
      min-free = ${toString (512 * 1024 * 1024)}
      warn-dirty = false
      log-lines = 50
    '';
  };
}
