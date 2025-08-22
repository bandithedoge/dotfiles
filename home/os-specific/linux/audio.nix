{
  pkgs,
  config,
  ...
}:
{
  home.packages = with pkgs; [
    ardour
    bandithedoge.cardinal
    bandithedoge.clap-info
    bandithedoge.partiels
    bandithedoge.projucer
    bandithedoge.snd2acm
    carla
    coppwr
    furnace
    picard
    pwvucontrol
    reaper

    # (yabridge.override {inherit (pkgs) wine;})
    # (yabridgectl.override {inherit (pkgs) wine;})
    # (airwin2rack.override { webkitgtk_4_0 = webkitgtk_4_1; })
    (chow-tape-model.override { webkitgtk_4_0 = webkitgtk_4_1; })
    (distrho-ports.override { plugins = [ "vitalium" ]; })
    aether-lv2
    bandithedoge.aida-x
    bandithedoge.blepfx.crunchrr
    bandithedoge.blepfx.destruqtor
    bandithedoge.blepfx.filtrr
    bandithedoge.cchorus
    bandithedoge.dsp56300
    bandithedoge.ildaeil
    bandithedoge.ircam-vamp-plugins-bin
    bandithedoge.lamb-bin
    bandithedoge.maim-bin
    bandithedoge.microbiome
    bandithedoge.mrugalla.hammer-and-meiszel
    bandithedoge.mrugalla.manta
    bandithedoge.mrugalla.nel-19
    bandithedoge.panacea-bin
    bandithedoge.peakeater-bin
    bandithedoge.plasma
    bandithedoge.reverse-camel
    bandithedoge.sapphire-plugins
    bandithedoge.sinuslabs.bandbreite
    bandithedoge.sinuslabs.ko
    bandithedoge.sinuslabs.reach
    bandithedoge.six-sines
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
    bandithedoge.unplugred
    bandithedoge.vitalium-verb-bin
    bandithedoge.white-elephant-audio.carve-bin
    bandithedoge.white-elephant-audio.monstr-bin
    bandithedoge.white-elephant-audio.richter-bin
    bandithedoge.white-elephant-audio.songbird-bin
    bandithedoge.zl-audio.inflator
    bandithedoge.zl-audio.warm
    calf
    decent-sampler
    dragonfly-reverb
    geonkick
    lsp-plugins
    master_me
  ];

  xdg.configFile = {
    "yabridgectl/config.toml".text = ''
      plugin_dirs = [
        '/mnt/soft/wine/drive_c/Program Files/Common Files/VST3',
        '/mnt/soft/wine/drive_c/Program Files/Common Files/CLAP',
        '/mnt/soft/wine/drive_c/Program Files/VstPlugins',
        '/mnt/soft/wine/drive_c/Program Files/Steinberg/VSTPlugins'
      ]
    '';
  };

  home.file = {
    ".vst3/yabridge/yabridge.toml".text = ''
      ["iZotope/*.vst3"]
      group = "izotope"
    '';

    ".BitwigStudio/graphics-backend".text = "skia-gl";

    ".u-he/ACE/Support/com.u-he.ACE.user.txt".source =
      config.lib.file.mkOutOfStoreSymlink
        config.sops.secrets."u-he/ace".path;
    ".u-he/Bazille/Support/com.u-he.Bazille.user.txt".source =
      config.lib.file.mkOutOfStoreSymlink
        config.sops.secrets."u-he/bazille".path;
    ".u-he/ColourCopy/Support/com.u-he.ColourCopy.user.txt".source =
      config.lib.file.mkOutOfStoreSymlink
        config.sops.secrets."u-he/colourcopy".path;
    ".u-he/Diva/Support/com.u-he.Diva.user.txt".source =
      config.lib.file.mkOutOfStoreSymlink
        config.sops.secrets."u-he/diva".path;
    ".u-he/Filterscape/Support/com.u-he.Filterscape.user.txt".source =
      config.lib.file.mkOutOfStoreSymlink
        config.sops.secrets."u-he/filterscape".path;
    ".u-he/Hive/Support/com.u-he.Hive.user.txt".source =
      config.lib.file.mkOutOfStoreSymlink
        config.sops.secrets."u-he/hive".path;
    ".u-he/MFM2/Support/com.u-he.MFM2.user.txt".source =
      config.lib.file.mkOutOfStoreSymlink
        config.sops.secrets."u-he/mfm2".path;
    ".u-he/Presswerk/Support/com.u-he.Presswerk.user.txt".source =
      config.lib.file.mkOutOfStoreSymlink
        config.sops.secrets."u-he/presswerk".path;
    ".u-he/Repro-1/Support/com.u-he.Repro-1.user.txt".source =
      config.lib.file.mkOutOfStoreSymlink
        config.sops.secrets."u-he/repro".path;
    ".u-he/Satin/Support/com.u-he.Satin.user.txt".source =
      config.lib.file.mkOutOfStoreSymlink
        config.sops.secrets."u-he/satin".path;
    ".u-he/Twangstrom/Support/com.u-he.Twangstrom.user.txt".source =
      config.lib.file.mkOutOfStoreSymlink
        config.sops.secrets."u-he/twangstrom".path;
    ".u-he/Uhbik/Support/com.u-he.Uhbik.user.txt".source =
      config.lib.file.mkOutOfStoreSymlink
        config.sops.secrets."u-he/uhbik".path;
    ".u-he/Zebra2/Support/com.u-he.Zebra2.user.txt".source =
      config.lib.file.mkOutOfStoreSymlink
        config.sops.secrets."u-he/zebra2".path;
  };
}
