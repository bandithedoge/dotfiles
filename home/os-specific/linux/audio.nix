{pkgs, ...}: {
  home.packages = with pkgs; [
    ardour
    bandithedoge.cardinal
    bandithedoge.clap-info
    bandithedoge.vgmtrans
    bandithedoge.zrythm
    carla
    furnace
    helvum
    # picard
    playerctl
    pwvucontrol
    reaper
    strawberry-qt6

    # plugins
    bandithedoge.airwindows-consolidated
    bandithedoge.digits-bin
    bandithedoge.geonkick
    bandithedoge.ildaeil
    # bandithedoge.lamb
    # bandithedoge.tal.chorus-lx
    bandithedoge.tal.drum
    bandithedoge.tal.j-8
    bandithedoge.tal.mod
    bandithedoge.tal.noisemaker
    bandithedoge.tal.reverb-4
    bandithedoge.tal.sampler
    bandithedoge.tal.u-no-lx
    distrho
    lsp-plugins
    plugdata
    surge-XT
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
