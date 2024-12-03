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
            gst_all_1.gst-plugins-bad
            gst_all_1.gst-plugins-ugly
          ];
        extraPreBwrapCmds = "touch /etc/NIXOS";
      };
      extraCompatPackages = with pkgs; [luxtorpeda proton-ge-custom steamtinkerlaunch];
    };

    gamescope = {
      enable = true;
      # capSysNice = true;
      package = pkgs.gamescope.overrideAttrs (_: {
        NIX_CFLAGS_COMPILE = ["-fno-fast-math"];
      });
    };

    anime-game-launcher.enable = true;
    gamemode = {
      enable = true;
      settings = {
        general.softrealtime = "auto";
        gpu = {
          apply_gpu_optimisations = "accept-responsibility";
          gpu_device = 0;
          amd_performance_level = "high";
        };
      };
    };
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
