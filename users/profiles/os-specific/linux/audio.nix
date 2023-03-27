{
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    ardour
    bandithedoge.cardinal
    bandithedoge.zrythm
    bespokesynth
    breeze-icons
    cadence
    carla
    ft2-clone
    furnace
    goattracker
    helvum
    milkytracker
    playerctl
    pt2-clone
    qpwgraph
    reaper
    soundtracker
    strawberry
    yabridge
    yabridgectl
    qtractor

    # plugins
    CHOWTapeModel
    ChowCentaur
    ChowKick
    adlplug
    aether-lv2
    airwindows-lv2
    ams-lv2
    artyFX
    autotalent
    bandithedoge.dpf-plugins
    bandithedoge.ildaeil
    bandithedoge.lv2vst
    bchoppr
    boops
    bristol
    calf
    caps
    delayarchitect
    distrho
    dragonfly-reverb
    drumgizmo
    drumkv1
    eq10q
    fmit
    geonkick
    guitarix
    gxplugins-lv2
    helm
    hydrogen
    lsp-plugins
    mod-distortion
    molot-lite
    mooSpace
    ninjas2
    qsampler
    samplv1
    surge-XT
    talentedhack
    tunefish
    vocproc
    wolf-shaper
    x42-avldrums
    x42-plugins
  ];

  services.easyeffects = {
    enable = true;
    preset = "main";
  };
}
