{pkgs, ...}: {
  home.packages = with pkgs; [
    ardour
    bandithedoge.cardinal
    bandithedoge.zrythm
    bespokesynth
    carla
    furnace
    giada
    picard
    playerctl
    pwvucontrol
    reaper
    strawberry

    # plugins
    ChowKick
    bandithedoge.distrho-ports
    bandithedoge.ildaeil
    bandithedoge.octasine
    bandithedoge.tal.bassline-101
    bandithedoge.tal.chorus-lx
    bandithedoge.tal.dac
    bandithedoge.tal.drum
    bandithedoge.tal.dub-x
    bandithedoge.tal.filter-2
    bandithedoge.tal.j-8
    bandithedoge.tal.mod
    bandithedoge.tal.noisemaker
    bandithedoge.tal.reverb-4
    bandithedoge.tal.sampler
    bandithedoge.tal.u-no-lx
    bandithedoge.tal.vocoder
    delayarchitect
    dragonfly-reverb
    eq10q
    geonkick
    lsp-plugins
    yabridge
    yabridgectl
  ];

  services.easyeffects = {
    enable = true;
    preset = "main";
  };

  xdg.configFile."yabridgectl/config.toml".text = ''
    plugin_dirs = [
      '/mnt/soft/Bottles/plugins/drive_c/Program Files/Common Files/VST3',
      '/mnt/soft/Bottles/plugins/drive_c/Program Files/Common Files/CLAP',
      '/mnt/soft/Bottles/plugins/drive_c/Program Files/VstPlugins',
      '/mnt/soft/Bottles/plugins/drive_c/Program Files/Steinberg/VSTPlugins'
    ]
  '';

  home.file.".vst3/yabridge/yabridge.toml".text = ''
    ["iZotope/*.vst3"]
    group = "izotope"
  '';
}
