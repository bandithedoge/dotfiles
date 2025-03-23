{
  pkgs,
  flake,
  lib,
  ...
}: {
  nix = {
    enable = true;
    package = lib.mkDefault pkgs.lix;
    settings = {
      auto-optimise-store = true;
      extra-experimental-features = ["nix-command" "flakes"];
      keep-failed = true;
      log-lines = 50;
      max-call-depth = 100000;
      trusted-users = ["root" "@wheel" "bandithedoge"];
      use-xdg-base-directories = true;
      warn-dirty = false;
      max-jobs = "auto";

      substituters = [
        "https://bandithedoge.cachix.org"
        "https://berberman.cachix.org"
        "https://cache.garnix.io"
        "https://cache.lix.systems"
        "https://cache.nixos.org"
        "https://chaotic-nyx.cachix.org/"
        "https://nix-community.cachix.org"
        "https://nix-gaming.cachix.org"
        "https://nixpkgs-unfree.cachix.org"
        "https://numtide.cachix.org"
      ];
      trusted-public-keys = [
        "bandithedoge.cachix.org-1:ZtcHw1anyEa4t6H8m3o/ctYFrwYFPAwoENSvofamE6g="
        "berberman.cachix.org-1:UHGhodNXVruGzWrwJ12B1grPK/6Qnrx2c3TjKueQPds="
        "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
        "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "chaotic-nyx.cachix.org-1:HfnXSw4pj95iI/n17rIDy40agHj12WfF+Gqk6SonIT8="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
        "nixpkgs-unfree.cachix.org-1:hqvoInulhbV4nJ9yJOEr+4wxhDV4xq2d1DK7S6Nj6rs="
        "numtide.cachix.org-1:2ps1kLBUWjxIneOy1Ik6cQjb41X0iXVXeHigGmycPPE="
      ];
    };
    nixPath = ["nixpkgs=${flake.inputs.nixpkgs}"];
  };
}
