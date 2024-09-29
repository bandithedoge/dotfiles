{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    steamtinkerlaunch
  ];

  programs = {
    steam = {
      enable = true;
      protontricks.enable = true;
      package = pkgs.steam.override {
        extraLibraries = pkgs:
          with pkgs; [
            xz
            openssl
          ];
        extraPreBwrapCmds = "touch /etc/NIXOS";
      };
      extraCompatPackages = with pkgs; [luxtorpeda proton-ge-custom steamtinkerlaunch];
    };
    gamescope = {
      enable = true;
      # capSysNice = true;
      package = pkgs.gamescope_git;
    };

    anime-game-launcher.enable = true;
    gamemode.enable = true;
    honkers-railway-launcher.enable = true;
  };

  security.pam.loginLimits = [
    {
      domain = "bandithedoge";
      type = "hard";
      item = "nofile";
      value = "524288";
    }
  ];
}
