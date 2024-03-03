{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    steamtinkerlaunch
  ];

  programs = {
    gamemode.enable = true;

    gamescope.enable = true;

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

    honkers-railway-launcher.enable = true;
    anime-game-launcher.enable = true;
  };

  chaotic.steam.extraCompatPackages = with pkgs; [luxtorpeda proton-ge-custom steamtinkerlaunch];
}
