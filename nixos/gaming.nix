{pkgs, ...}: {
  security.pam.loginLimits = [
    {
      domain = "bandithedoge";
      type = "hard";
      item = "nofile";
      value = "524288";
    }
  ];

  programs.gamescope = {
    enable = true;
    package = pkgs.gamescope_git;
    capSysNice = true;
  };

  programs.gamemode.enable = true;
  programs.steam = {
    enable = true;
    package = pkgs.steam.override {
      extraPkgs = pkgs: with pkgs; [xz];
    };
  };

  chaotic.steam.extraCompatPackages = with pkgs; [luxtorpeda proton-ge-custom steamtinkerlaunch];
}
