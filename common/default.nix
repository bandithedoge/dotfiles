{ pkgs, ... }: {
  nix.package = pkgs.nixUnstable;

  users.users."bandithedoge".name = "bandithedoge";
  environment.shells = [ pkgs.fish pkgs.bashInteractive ];

  programs.fish.enable = true;
}
