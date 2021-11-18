{ pkgs, ... }: {
  nix = {
    package = pkgs.nixUnstable;
    binaryCaches = [ "https://nix-community.cachix.org" ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    extraOptions = ''
      extra-experimental-features = nix-command flakes
    '';
  };

  nixpkgs.config.allowBroken = true;

  users.users."bandithedoge".name = "bandithedoge";
  environment.shells = [ pkgs.fish pkgs.bashInteractive ];

  programs.fish.enable = true;
}
