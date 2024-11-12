{pkgs, ...}: {
  home.packages = with pkgs; [
    ardour
    bandithedoge.cardinal
    bandithedoge.clap-info
    bandithedoge.vgmtrans
    carla
    coppwr
    furnace
    mpc-cli
    picard
    playerctl
    pwvucontrol
    reaper
    strawberry-qt6

    (bandithedoge.the-usual-suspects.osirus.override {
      rom = pkgs.fetchurl {
        url = "https://archive.org/download/access-virus-b-c-roms/Access%20Virus%20B%20%28am29f040b_4v9%29.zip/Access%20Virus%20B%20%28am29f040b_4v9%29.BIN";
        sha256 = "0br4c84y19igpr883m7dkrwsav9qh90965ri2rqwmzk52hmdgkkw";
        name = "firmware.bin";
      };
    })
    (bandithedoge.the-usual-suspects.ostirus.override {
      rom = pkgs.fetchurl {
        url = "https://archive.org/download/firmware_20240515/firmware.bin";
        sha256 = "0sndnl7s1nr4f7y83bw6dr0cy79rwy0mzfq5f8dwp7iganlawgcf";
      };
    })
    bandithedoge.airwindows-consolidated
    bandithedoge.destruqtor
    bandithedoge.geonkick
    bandithedoge.ildaeil
    bandithedoge.microbiome-bin
    bandithedoge.misstrhortion
    bandithedoge.panacea-bin
    bandithedoge.tonelib.bassdrive
    bandithedoge.tonelib.easycomp
    bandithedoge.tonelib.noisereducer
    bandithedoge.tonelib.tubewarmth
    bandithedoge.uhhyou
    bandithedoge.white-elephant-audio.carve-bin
    bandithedoge.white-elephant-audio.monstr-bin
    bandithedoge.white-elephant-audio.richter-bin
    bandithedoge.white-elephant-audio.songbird-bin
    lsp-plugins
    plugdata
    surge-XT
  ];

  services = {
    easyeffects = {
      enable = true;
      preset = "main";
    };
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
