{ pkgs, ... }: {
  nix = {
    package = pkgs.nixUnstable;
    binaryCaches =
      [ "https://nix-community.cachix.org" "https://hydra.iohk.io" ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    extraOptions = ''
      extra-experimental-features = nix-command flakes
    '';
    trustedUsers = [ "root" "@wheel" "bandithedoge" ];
  };

  nixpkgs.config.allowBroken = true;

  users.users."bandithedoge" = {
    name = "bandithedoge";
    group = "bandithedoge";
    home = "/home/bandithedoge";
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    shell = pkgs.fish;
  };
  environment.shells = [ pkgs.fish pkgs.bashInteractive ];

  programs.fish.enable = true;
}
