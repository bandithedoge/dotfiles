{
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    ardour
    bandithedoge.cardinal
    bandithedoge.zrythm
    bespokesynth
    carla
    furnace
    playerctl
    reaper
    strawberry

    # plugins
    ChowKick
    bandithedoge.ildaeil
    delayarchitect
    dragonfly-reverb
    geonkick
    guitarix
    lsp-plugins
    yabridge
    yabridgectl
  ];

  services.easyeffects = {
    enable = true;
    preset = "main";
  };

  xdg.configFile."yabridgectl/config.toml".text = ''
    plugin_dirs = ['/mnt/soft/Bottles/audio/drive_c/Program Files/Common Files/VST3', '/mnt/soft/Bottles/audio/drive_c/Program Files/Common Files/CLAP']

    ["iZotope*.vst3"]
    group = "izotope"
  '';
}
