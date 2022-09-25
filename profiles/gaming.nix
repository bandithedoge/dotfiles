{
  config,
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

  programs.gamemode = {
    enable = true;
  };
}
