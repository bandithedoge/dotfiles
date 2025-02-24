{pkgs, ...}: {
  programs = {
    steam = {
      enable = true;
      protontricks.enable = true;
      remotePlay.openFirewall = true;
      package = pkgs.steam.override {
        extraLibraries = pkgs:
          with pkgs; [
            xz
            openssl
            gst_all_1.gstreamer
            gst_all_1.gst-plugins-bad
            gst_all_1.gst-plugins-ugly
            gst_all_1.gst-plugins-good
          ];
        extraPreBwrapCmds = "touch /etc/NIXOS";
        # https://github.com/NixOS/nixpkgs/issues/338266#issuecomment-2419568331
        extraBwrapArgs = ["--unsetenv TZ"];
      };
      extraCompatPackages = with pkgs; [
        proton-ge-custom
      ];
    };

    gamescope = {
      enable = true;
      # capSysNice = true;
    };

    # gamemode = {
    #   enable = true;
    #   settings = {
    #     general = {
    #       softrealtime = "auto";
    #       renice = 10;
    #     };
    #     gpu = {
    #       apply_gpu_optimisations = "accept-responsibility";
    #       gpu_device = 0;
    #       amd_performance_level = "high";
    #     };
    #   };
    # };
  };

  security.pam.loginLimits = [
    {
      domain = "bandithedoge";
      type = "hard";
      item = "nofile";
      value = "524288";
    }
  ];

  networking.mihoyo-telemetry.block = true;

  boot.kernel.sysctl = {
    "kernel.sched_cfs_bandwidth_slice_us" = 3000;
    "net.ipv4.tcp_fin_timeout" = 5;
    "vm.max_map_count" = 2147483642;
  };
}
