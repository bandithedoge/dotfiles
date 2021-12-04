{ pkgs, ... }: {
  nix = {
    package = pkgs.nixUnstable;
    binaryCaches = [
      "https://nix-community.cachix.org"
      "https://hydra.iohk.io"
      "https://cache.nixos.org"
      "https://nixpkgs-wayland.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
    ];
    extraOptions = ''
      extra-experimental-features = nix-command flakes
    '';
    trustedUsers = [ "root" "@wheel" "bandithedoge" ];
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
    (nerdfonts.override { fonts = [ "FiraCode" ]; })
    roboto
  ];

  programs.fish.enable = true;
}
