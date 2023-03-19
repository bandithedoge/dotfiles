{
  pkgs,
  hmUsers,
  ...
}: {
  home-manager.users = {inherit (hmUsers) bandithedoge;};

  users.users."bandithedoge" = {
    name = "bandithedoge";
    shell = pkgs.fish;
    extraGroups = ["wheel" "networkmanager" "audio" "adbusers" "plugdev"];
    isNormalUser = true;
    uid = 1000;
  };
}
