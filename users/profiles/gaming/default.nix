{
  config,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    (lutris.override {extraLibraries = pkgs: with pkgs; [mangohud];})
    gzdoom
    polymc
  ];

  programs.mangohud = {
    enable = true;
    enableSessionWide = true;
    settings = {
      fps_limit = 0;
      vsync = 1;
      toggle_hud = "F12";

      version = true;
      gpu_stats = true;
      cpu_stats = true;
      ram = true;
      fps = true;
      frametime = true;
      gpu_name = true;
      vulkan_driver = true;
      wine = true;
      frame_timing = true;
      gamemode = true;
      show_fps_limit = true;
      resolution = true;
    };
  };
}
