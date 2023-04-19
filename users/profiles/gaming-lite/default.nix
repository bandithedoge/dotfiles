{pkgs, ...}: {
  home.packages = with pkgs; [
    (lutris.override {extraLibraries = pkgs: with pkgs; [mangohud];})
    bastet
    nethack
    opentyrian
    unnethack
    vitetris
  ];

  programs.mangohud = {
    enable = true;
    enableSessionWide = true;
    settings = {
      fps_limit = 0;
      vsync = 1;
      gl_vsync = 0;

      round_corners = 5;
      hud_compact = true;
      table_columns = 4;

      toggle_hud = "Shift_L+F12";
      toggle_hud_position = "Shift_L+F11";
      toggle_fps_limit = "Shift_L+F1";

      time = true;
      version = true;
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

      gpu_name = true;
      vulkan_driver = true;
      wine = true;
      gamemode = true;
      vkbasalt = true;
      arch = true;
      show_fps_limit = true;
      resolution = true;
    };
  };
}
