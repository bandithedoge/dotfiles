{pkgs, ...}: {
  home.packages = with pkgs; [
    ardour
    bandithedoge.cardinal
    bandithedoge.zrythm
    carla
    picard
    playerctl
    # pwvucontrol # HACK: https://github.com/NixOS/nixpkgs/issues/297574
    reaper
    strawberry

    # plugins
    ChowKick
    bandithedoge.distrho-ports
    bandithedoge.geonkick
    bandithedoge.ildaeil
    bandithedoge.tal.chorus-lx
    bandithedoge.tal.drum
    bandithedoge.tal.j-8
    bandithedoge.tal.mod
    bandithedoge.tal.noisemaker
    bandithedoge.tal.reverb-4
    bandithedoge.tal.sampler
    bandithedoge.tal.u-no-lx
    dragonfly-reverb
    eq10q
    lsp-plugins
  ];

  services.easyeffects = {
    enable = true;
    preset = "main";
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
