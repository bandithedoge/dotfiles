{pkgs, ...}: {
  home.packages = with pkgs; [
    ardour
    bandithedoge.cantata
    bandithedoge.cardinal
    bandithedoge.clap-info
    bandithedoge.vgmtrans
    carla
    furnace
    mpc-cli
    picard
    playerctl
    pwvucontrol
    reaper
    strawberry-qt6
    coppwr

    bandithedoge.airwindows-consolidated
    bandithedoge.digits-bin
    bandithedoge.geonkick
    bandithedoge.ildaeil
    distrho
    lsp-plugins
    plugdata
    surge-XT
  ];

  services = {
    easyeffects = {
      enable = true;
      preset = "main";
    };

    mpd = {
      enable = true;
      musicDirectory = "/mnt/data/Music/Music";
      extraConfig = ''
        audio_output {
          type "pipewire"
          name "default"
        }
      '';
      network.startWhenNeeded = true;
    };

    mpd-discord-rpc = {
      enable = true;
      settings = {
        format = {
          details = "$artist - $title";
          state = "$album";
          large_text = "$date";
          small_image = "";
        };
      };
    };
  };

  xdg.configFile."yabridgectl/config.toml".text = ''
    plugin_dirs = [
      '/mnt/soft/Bottles/plugins_bottles/drive_c/Program Files/Common Files/VST3',
      '/mnt/soft/Bottles/plugins_bottles/drive_c/Program Files/Common Files/CLAP',
      '/mnt/soft/Bottles/plugins_bottles/drive_c/Program Files/VstPlugins',
      '/mnt/soft/Bottles/plugins_bottles/drive_c/Program Files/Steinberg/VSTPlugins'
    ]
  '';

  home.file.".vst3/yabridge/yabridge.toml".text = ''
    ["iZotope/*.vst3"]
    group = "izotope"
  '';
}
