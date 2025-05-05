{
  pkgs,
  config,
  lib,
  ...
}: {
  home.packages = with pkgs; [
    (audacious.override {withPlugins = true;})
    ardour
    bandithedoge.clap-info
    bandithedoge.partiels
    cardinal
    carla
    coppwr
    furnace
    picard
    playerctl
    pwvucontrol
    reaper
    strawberry-qt6

    (bandithedoge.distrho-ports.override {plugins = ["vitalium"];})
    (yabridge.override {inherit (pkgs) wine;})
    (yabridgectl.override {inherit (pkgs) wine;})
    aether-lv2
    bandithedoge.airwindows-consolidated
    bandithedoge.blepfx.crunchrr
    bandithedoge.blepfx.destruqtor
    bandithedoge.blepfx.filtrr
    bandithedoge.dsp56300
    bandithedoge.element
    bandithedoge.geonkick
    bandithedoge.gnomedistort2
    bandithedoge.guitarix-vst-bin
    bandithedoge.ildaeil
    bandithedoge.ircam-vamp-plugins-bin
    bandithedoge.k-whooms
    bandithedoge.lamb-bin
    bandithedoge.maim-bin
    bandithedoge.microbiome-bin
    bandithedoge.panacea-bin
    bandithedoge.peakeater-bin
    bandithedoge.reverse-camel
    bandithedoge.ripplerx
    bandithedoge.rnnoise-plugin
    bandithedoge.roomreverb
    bandithedoge.schrammel-ojd
    bandithedoge.sg-323
    bandithedoge.showmidi-bin
    bandithedoge.squeezer-bin
    bandithedoge.u-he.ace
    bandithedoge.u-he.bazille
    bandithedoge.u-he.colourcopy
    bandithedoge.u-he.diva
    bandithedoge.u-he.filterscape
    bandithedoge.u-he.hive
    bandithedoge.u-he.mfm2
    bandithedoge.u-he.podolski
    bandithedoge.u-he.presswerk
    bandithedoge.u-he.protoverb
    bandithedoge.u-he.repro
    bandithedoge.u-he.satin
    bandithedoge.u-he.triplecheese
    bandithedoge.u-he.twangstrom
    bandithedoge.u-he.uhbik
    bandithedoge.u-he.zebra-legacy
    bandithedoge.valentine
    bandithedoge.vitalium-verb-bin
    bandithedoge.white-elephant-audio.carve-bin
    bandithedoge.white-elephant-audio.monstr-bin
    bandithedoge.white-elephant-audio.richter-bin
    bandithedoge.white-elephant-audio.songbird-bin
    bandithedoge.zl-audio.inflator
    bandithedoge.zl-audio.warm
    calf
    chow-tape-model
    dragonfly-reverb
    lsp-plugins
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

  home.file = {
    ".vst3/yabridge/yabridge.toml".text = ''
      ["iZotope/*.vst3"]
      group = "izotope"
    '';

    ".u-he/ACE/Support/com.u-he.ACE.user.txt".source = config.lib.file.mkOutOfStoreSymlink config.sops.secrets."u-he/ace".path;
    ".u-he/Bazille/Support/com.u-he.Bazille.user.txt".source = config.lib.file.mkOutOfStoreSymlink config.sops.secrets."u-he/bazille".path;
    ".u-he/ColourCopy/Support/com.u-he.ColourCopy.user.txt".source = config.lib.file.mkOutOfStoreSymlink config.sops.secrets."u-he/colourcopy".path;
    ".u-he/Diva/Support/com.u-he.Diva.user.txt".source = config.lib.file.mkOutOfStoreSymlink config.sops.secrets."u-he/diva".path;
    ".u-he/Filterscape/Support/com.u-he.Filterscape.user.txt".source = config.lib.file.mkOutOfStoreSymlink config.sops.secrets."u-he/filterscape".path;
    ".u-he/Hive/Support/com.u-he.Hive.user.txt".source = config.lib.file.mkOutOfStoreSymlink config.sops.secrets."u-he/hive".path;
    ".u-he/MFM2/Support/com.u-he.MFM2.user.txt".source = config.lib.file.mkOutOfStoreSymlink config.sops.secrets."u-he/mfm2".path;
    ".u-he/Presswerk/Support/com.u-he.Presswerk.user.txt".source = config.lib.file.mkOutOfStoreSymlink config.sops.secrets."u-he/presswerk".path;
    ".u-he/Repro-1/Support/com.u-he.Repro-1.user.txt".source = config.lib.file.mkOutOfStoreSymlink config.sops.secrets."u-he/repro".path;
    ".u-he/Satin/Support/com.u-he.Satin.user.txt".source = config.lib.file.mkOutOfStoreSymlink config.sops.secrets."u-he/satin".path;
    ".u-he/Twangstrom/Support/com.u-he.Twangstrom.user.txt".source = config.lib.file.mkOutOfStoreSymlink config.sops.secrets."u-he/twangstrom".path;
    ".u-he/Uhbik/Support/com.u-he.Uhbik.user.txt".source = config.lib.file.mkOutOfStoreSymlink config.sops.secrets."u-he/uhbik".path;
    ".u-he/Zebra2/Support/com.u-he.Zebra2.user.txt".source = config.lib.file.mkOutOfStoreSymlink config.sops.secrets."u-he/zebra2".path;
  };
}
