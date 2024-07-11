{
  pkgs,
  ...
}: {
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
          "plugdev"
          "vboxusers"
          "wheel"
          "kvm"
        ];
        isNormalUser = true;
      }
      else {
        home = "/Users/bandithedoge";
      }
    );

  environment.shells = with pkgs; [fish bashInteractive];
  programs.fish.enable = true;

  home-manager.users.bandithedoge.imports = [../home/default];
}

