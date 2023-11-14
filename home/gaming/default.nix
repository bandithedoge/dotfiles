{
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    bandithedoge.raze
    gzdoom
    bastet
    lutris
    nethack
    opentyrian
    unnethack
    vitetris
  ];

  programs.mangohud = {
    enable = true;
    enableSessionWide = true;
    package = pkgs.mangohud_git;
    settings = {
      fps_limit = 0;
      vsync = 1;
      gl_vsync = 0;

      round_corners = 5;
      hud_compact = true;
      table_columns = 4;

      toggle_fps_limit = "Shift_R+F1";
      toggle_logging = "Shift_R+F2";
      upload_log = "Shift_R+F3";
      reload_cfg = "Shift_R+F4";
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
      histogram = true;
      frametime = true;
      frame_timing = true;
      battery = true;
      battery_icon = true;

      engine_version = true;
      gpu_name = true;
      vulkan_driver = true;
      wine = true;
      gamemode = true;
      show_fps_limit = true;
      resolution = true;
    };
  };
}
