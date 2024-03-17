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

    anime-game-launcher.enable = true;
    gamemode.enable = true;
    gamescope.enable = true;
    honkers-railway-launcher.enable = true;
  };

  chaotic.steam.extraCompatPackages = with pkgs; [luxtorpeda proton-ge-custom steamtinkerlaunch];
}
