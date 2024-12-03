{pkgs, ...}: {
  home.packages = with pkgs; [
    ardour
    bandithedoge.cardinal
    bandithedoge.clap-info
    bandithedoge.vgmtrans
    bespokesynth-with-vst2
    carla
    coppwr
    furnace
    mpc-cli
    picard
    playerctl
    pwvucontrol
    reaper
    strawberry-qt6

    bandithedoge.aida-x
    bandithedoge.airwindows-consolidated
    bandithedoge.audible-planets-bin
    bandithedoge.destruqtor
    bandithedoge.dsp56300
    bandithedoge.geonkick
    bandithedoge.gnomedistort2
    bandithedoge.guitarix-vst-bin
    bandithedoge.ildaeil
    bandithedoge.microbiome-bin
    bandithedoge.misstrhortion
    bandithedoge.panacea-bin
    bandithedoge.peakeater-bin
    bandithedoge.roomreverb
    bandithedoge.schrammel-ojd
    bandithedoge.sg-323
    bandithedoge.showmidi-bin
    bandithedoge.tonelib.bassdrive
    bandithedoge.tonelib.easycomp
    bandithedoge.tonelib.noisereducer
    bandithedoge.tonelib.tubewarmth
    bandithedoge.tonez
    bandithedoge.white-elephant-audio.carve-bin
    bandithedoge.white-elephant-audio.monstr-bin
    bandithedoge.white-elephant-audio.richter-bin
    bandithedoge.white-elephant-audio.songbird-bin
    guitarix
    lsp-plugins
    odin2
    plugdata
    surge-XT
  ];

  services.easyeffects.enable = true;

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
