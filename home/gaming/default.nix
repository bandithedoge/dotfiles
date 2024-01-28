{pkgs, ...}: {
  home.packages = with pkgs; [
    (lutris.override {
      extraPkgs = pkgs':
        with pkgs'; [
          gamescope
          gamemode
        ];
      extraLibraries = pkgs':
        with pkgs'; [
          nspr
          xorg.libXScrnSaver
          xorg.libXdamage
          xorg.libXfixes
          xorg.libXft
          libadwaita
          pango
          gtk4
        ];
    })
    bandithedoge.raze
    bastet
    gamescope
    gzdoom
    nethack
    openmw
    opentyrian
    prismlauncher
    unnethack
    vitetris
    vulkanPackages_latest.vulkan-tools
  ];

  programs.mangohud = {
    enable = true;
    enableSessionWide = true;
    package = pkgs.mangohud_git;
    settings = {
      fps_limit = "0,60";
      vsync = 1;
      gl_vsync = 0;

      round_corners = 5;
      table_columns = 4;

      toggle_fps_limit = "Shift_R+F10";
      toggle_hud_position = "Shift_R+F11";
      toggle_hud = "Shift_R+F12";

      time = true;
      cpu_stats = true;
      cpu_temp = true;
      cpu_mhz = true;
      gpu_stats = true;
      gpu_temp = true;
      gpu_core_clock = true;
      vram = true;
      ram = true;
      swap = true;
      procmem = true;
      fps = true;
      fps_color_change = true;
      histogram = true;
      frametime = true;
      frame_timing = true;
      battery = true;
      battery_icon = true;

      engine_version = true;
      gpu_name = true;
      exec_name = true;
      vulkan_driver = true;
      wine = true;
      fsr = true;
      gamemode = true;
      show_fps_limit = true;
      resolution = true;
    };
  };
}
