{pkgs, ...}: {
  home = {
    packages = with pkgs; [
      bandithedoge.nugget-doom
      bandithedoge.vkdoom
      doomrunner
      lutris
      pcsx2
      prismlauncher
      umu-launcher
      vulkanPackages_latest.vulkan-tools
    ];
  };

  services.flatpak = {
    remotes = [
      {
        name = "launcher.moe";
        location = "https://gol.launcher.moe/gol.launcher.moe.flatpakrepo";
      }
    ];
    packages = [
      {
        appId = "moe.launcher.an-anime-game-launcher";
        origin = "launcher.moe";
      }
      {
        appId = "moe.launcher.the-honkers-railway-launcher";
        origin = "launcher.moe";
      }
      {
        appId = "org.openmw.OpenMW";
        origin = "flathub-beta";
      }
    ];
    overrides = {
      "org.openmw.OpenMW" = {
        Context.sockets = ["wayland" "!x11" "!fallback-x11"];
        Environment.OSG_VERTEX_BUFFER_HINT = "VERTEX_BUFFER_OBJECT";
      };
    };
  };

  programs.mangohud = {
    enable = true;
    # enableSessionWide = true;
    package = pkgs.mangohud_git;
    settings = let
      color = pkgs.lib.removePrefix "#";
    in
      with pkgs.rice; rec {
        fps_limit = [0 60];
        vsync = 1;
        gl_vsync = 0;

        round_corners = 5;
        table_columns = 4;

        toggle_fps_limit = "Shift_R+F10";
        toggle_hud_position = "Shift_R+F11";
        toggle_hud = "Shift_R+F12";

        time = true;
        time_no_label = true;

        cpu_stats = true;
        cpu_temp = true;
        cpu_mhz = true;
        cpu_load_value = [60 90];
        cpu_load_color = map color [base0B base0A base08];

        gpu_stats = true;
        gpu_temp = true;
        gpu_core_clock = true;
        gpu_load_value = cpu_load_value;
        gpu_load_color = cpu_load_color;

        vram = true;
        ram = true;
        swap = true;
        procmem = true;

        fps = true;
        fps_color_change = true;
        fps_value = [30 60];
        fps_color = map color [base08 base0A base0B];

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
        winesync = true;
        fsr = true;
        display_server = true;
        arch = true;
        gamemode = true;
        show_fps_limit = true;
        resolution = true;

        text_outline = false;
        text_color = color base05;
        gpu_color = color base0B;
        cpu_color = color base0C;
        vram_color = color base0D;
        ram_color = color base0E;
        engine_color = color base05;
        frametime_color = color base0F;
        background_color = color base00;
        battery_color = color base08;
      };
  };

  xdg.dataFile = {
    "Steam/compatibilitytools.d/proton-ge".source = pkgs.bandithedoge.proton.ge.steamcompattool;
    "Steam/compatibilitytools.d/proton-sarek".source = pkgs.bandithedoge.proton.sarek.steamcompattool;
  };
}
