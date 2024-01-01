{pkgs, ...}: {
  home.packages = with pkgs; [
    ardour
    bandithedoge.cardinal
    bandithedoge.zrythm
    bespokesynth
    carla
    furnace
    giada
    playerctl
    pwvucontrol
    reaper
    strawberry

    # plugins
    ChowKick
    bandithedoge.distrho-ports
    bandithedoge.ildaeil
    bandithedoge.octasine
    delayarchitect
    dragonfly-reverb
    eq10q
    geonkick
    guitarix
    lsp-plugins
    open-music-kontrollers.mephisto
    yabridge
    yabridgectl
  ];

  services.easyeffects = {
    enable = true;
    preset = "main";
  };

  xdg.configFile."yabridgectl/config.toml".text = ''
    plugin_dirs = [
      '/mnt/soft/Bottles/audio/drive_c/Program Files/Common Files/VST3',
      '/mnt/soft/Bottles/audio/drive_c/Program Files/Common Files/CLAP',
      '/mnt/soft/Bottles/audio/drive_c/Program Files/VstPlugins',
      '/mnt/soft/Bottles/audio/drive_c/Program Files/Steinberg/VSTPlugins'
    ]
  '';

  home.file.".vst3/yabridge/yabridge.toml".text = ''
    ["iZotope/*.vst3"]
    group = "izotope"
  '';
}
