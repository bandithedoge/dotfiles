{pkgs, hmUsers, ...}: {
  home-manager.users = {inherit (hmUsers) bandithedoge-wsl;};

  users.users."bandithedoge-wsl" = rec {
    name = "bandithedoge";
    shell = pkgs.fish;
    extraGroups = ["wheel"];
    isNormalUser = true;
    home = "/mnt/c/Users/${name}";
  };
}