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

    # (bandithedoge.arboreal.omniamp.override {noLicenseCheck = true;})
    # (bandithedoge.arboreal.pimax.override {noLicenseCheck = true;})
    bandithedoge.aida-x
    bandithedoge.airwindows-consolidated
    bandithedoge.apisonic.transperc
    bandithedoge.arboreal.str-x
    bandithedoge.audible-planets-bin
    bandithedoge.destruqtor
    bandithedoge.dsp56300
    bandithedoge.geonkick
    bandithedoge.gnomedistort2
    bandithedoge.guitarix-vst-bin
    # bandithedoge.guitarml.chameleon
    # bandithedoge.guitarml.prince
    # bandithedoge.guitarml.proteus
    # bandithedoge.guitarml.smartamp
    # bandithedoge.guitarml.smartpedal
    # bandithedoge.guitarml.ts-m1n3
    bandithedoge.ildaeil
    bandithedoge.maim-bin
    bandithedoge.microbiome-bin
    bandithedoge.misstrhortion
    bandithedoge.mxtune-bin
    bandithedoge.panacea-bin
    bandithedoge.peakeater-bin
    bandithedoge.rnnoise-plugin
    bandithedoge.roomreverb
    bandithedoge.schrammel-ojd
    bandithedoge.sg-323
    bandithedoge.showmidi-bin
    bandithedoge.squeezer-bin
    bandithedoge.tonelib.bassdrive
    bandithedoge.tonelib.easycomp
    bandithedoge.tonelib.noisereducer
    bandithedoge.tonelib.tubewarmth
    bandithedoge.venn.free-suite
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
