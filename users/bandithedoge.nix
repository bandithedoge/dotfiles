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
        extraGroups = [
          "adbusers"
          "audio"
          "docker"
          "networkmanager"
          "plugdev"
          "vboxusers"
          "wheel"
        ];
        isNormalUser = true;
      }
      else {
        home = "/Users/bandithedoge";
      }
    );

  environment.shells = with pkgs; [fish bashInteractive];
  programs.fish.enable = true;
}
