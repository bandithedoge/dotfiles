{
  pkgs,
  home-manager,
  ...
}: {
  home.packages = with pkgs; [
    ardour
    bandithedoge.zrythm
    bespokesynth
    cadence
    cardinal
    carla
    ft2-clone
    goattracker
    helvum
    milkytracker
    pt2-clone
    qpwgraph
    reaper
    soundtracker

    # plugins
    aether-lv2
    autotalent
    calf
    cardinal
    distrho
    geonkick
  ];

  services.easyeffects = {
    enable = true;
    preset = "main";
  };
}
