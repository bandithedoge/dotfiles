{
  pkgs,
  lib,
  inputs,
  ...
}: {
  nix = {
    enable = true;
    package = lib.mkDefault pkgs.lixPackageSets.latest.lix;
    settings = {
      auto-optimise-store = true;
      extra-experimental-features = [
        "flakes"
        "lix-custom-sub-commands"
        "nix-command"
        "no-url-literals"
      ];
      keep-failed = true;
      log-lines = 50;
      max-call-depth = 100000;
      trusted-users = ["root" "@wheel" "bandithedoge"];
      use-xdg-base-directories = true;
      warn-dirty = false;

      substituters = [
        "https://bandithedoge.cachix.org"
        "https://cache.nixos.org"
        "https://chaotic-nyx.cachix.org/"
        "https://nix-community.cachix.org"
        "https://nix-gaming.cachix.org"
      ];
      trusted-public-keys = [
        "bandithedoge.cachix.org-1:ZtcHw1anyEa4t6H8m3o/ctYFrwYFPAwoENSvofamE6g="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "chaotic-nyx.cachix.org-1:HfnXSw4pj95iI/n17rIDy40agHj12WfF+Gqk6SonIT8="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
      ];
    };
    nixPath = ["nixpkgs=${inputs.nixpkgs}"];
  };
}
