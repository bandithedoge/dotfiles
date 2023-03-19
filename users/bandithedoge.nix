{
  pkgs,
  hmUsers,
  ...
}: {
  home-manager.users = {inherit (hmUsers) bandithedoge;};

  users.users."bandithedoge" =
    {
      name = "bandithedoge";
      shell = pkgs.fish;
      uid = 1000;
    }
    // (
      if !pkgs.stdenv.isDarwin
      then {
        extraGroups = ["wheel" "networkmanager" "audio" "adbusers" "plugdev" "vboxusers"];
        isNormalUser = true;
      }
      else {
        home = "/Users/bandithedoge";
      }
    );

  environment.shells = with pkgs; [fish bashInteractive];
}
