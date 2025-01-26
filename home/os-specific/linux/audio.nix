{pkgs, ...}: {
  home.packages = with pkgs; [
    # ardour # https://github.com/NixOS/nixpkgs/pull/376823
    bandithedoge.cardinal
    bandithedoge.clap-info
    bandithedoge.partiels
    bandithedoge.vgmtrans
    bandithedoge.zrythm
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

    (bandithedoge.distrho-ports.override {plugins = ["vitalium"];})
    (yabridge.override {wine = pkgs.wine-ge;})
    (yabridgectl.override {wine = pkgs.wine-ge;})
    aether-lv2
    bandithedoge.aida-x
    bandithedoge.airwindows-consolidated
    bandithedoge.apisonic.transperc
    bandithedoge.arboreal.str-x
    bandithedoge.audible-planets-bin
    bandithedoge.blepfx.crunchrr
    bandithedoge.blepfx.destruqtor
    bandithedoge.crepe-vamp-plugin-bin
    bandithedoge.dsp56300
    bandithedoge.geonkick
    bandithedoge.gnomedistort2
    bandithedoge.guitarix-vst-bin
    bandithedoge.igorski.darvaza
    bandithedoge.igorski.delirion
    bandithedoge.igorski.fogpad
    bandithedoge.igorski.homecorrupter
    bandithedoge.igorski.rechoir
    bandithedoge.igorski.regrader
    bandithedoge.igorski.transformant
    bandithedoge.igorski.vstsid
    bandithedoge.ildaeil
    bandithedoge.ircam-vamp-plugins-bin
    bandithedoge.just-a-sample-bin
    bandithedoge.lamb-bin
    bandithedoge.maim-bin
    bandithedoge.microbiome-bin
    bandithedoge.misstrhortion
    bandithedoge.molot-lite
    bandithedoge.monique
    bandithedoge.mxtune-bin
    bandithedoge.panacea-bin
    bandithedoge.peakeater-bin
    bandithedoge.reverse-camel
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
    bandithedoge.valentine
    bandithedoge.venn.free-suite
    bandithedoge.vitalium-verb-bin
    bandithedoge.whisper-vamp-plugin-bin.out
    bandithedoge.white-elephant-audio.carve-bin
    bandithedoge.white-elephant-audio.monstr-bin
    bandithedoge.white-elephant-audio.richter-bin
    bandithedoge.white-elephant-audio.songbird-bin
    dragonfly-reverb
    guitarix
    lsp-plugins
    plugdata
    sfizz
    surge-XT
    x42-plugins
    yoshimi
    zam-plugins
  ];

  services.easyeffects.enable = true;

  xdg.configFile."yabridgectl/config.toml".text = ''
    plugin_dirs = [
      '/mnt/soft/wine/drive_c/Program Files/Common Files/VST3',
      '/mnt/soft/wine/drive_c/Program Files/Common Files/CLAP',
      '/mnt/soft/wine/drive_c/Program Files/VstPlugins',
      '/mnt/soft/wine/drive_c/Program Files/Steinberg/VSTPlugins'
    ]
  '';

  home.file.".vst3/yabridge/yabridge.toml".text = ''
    ["iZotope/*.vst3"]
    group = "izotope"
  '';
}
