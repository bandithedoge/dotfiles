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
      isHidden = false;
    }
    // (
      if !pkgs.stdenv.isDarwin
      then {
        isNormalUser = true;
        extraGroups = ["wheel" "networkmanager" "audio" "adbusers" "plugdev" "vboxusers"];
      }
      else {
        home = "/Users/bandithedoge";
      }
    );

  environment.shells = with pkgs; [fish bashInteractive];
}
