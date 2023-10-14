{
  pkgs,
  ...
}: {
  security.pam.loginLimits = [
    {
      domain = "bandithedoge";
      type = "hard";
      item = "nofile";
      value = "524288";
    }
  ];

  programs.gamemode.enable = true;

  programs.gamescope = {
    enable = true;
    package = pkgs.gamescope_git;
  };
}
