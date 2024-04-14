{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    steamtinkerlaunch
  ];

  programs = {
    steam = {
      enable = true;
      package = pkgs.steam.override {
        extraLibraries = pkgs:
          with pkgs; [
            xz
            openssl
          ];
      };
    };
    gamescope = {
      enable = true;
      # capSysNice = true;
    };

    anime-game-launcher.enable = true;
    gamemode.enable = true;
    honkers-railway-launcher.enable = true;
  };

  chaotic.steam.extraCompatPackages = with pkgs; [luxtorpeda proton-ge-custom steamtinkerlaunch];

  security.pam.loginLimits = [
    {
      domain = "bandithedoge";
      type = "hard";
      item = "nofile";
      value = "524288";
    }
  ];
}
