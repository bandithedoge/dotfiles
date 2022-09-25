{
  pkgs,
  home-manager,
  ...
}: {
  home.packages = with pkgs; [
    ardour
    bandithedoge.zrythm
    bespokesynth
    breeze-icons
    cadence
    cardinal
    carla
    ft2-clone
    goattracker
    helvum
    milkytracker
    playerctl
    pt2-clone
    qpwgraph
    reaper
    soundtracker
    strawberry
    furnace

    # plugins
    ChowKick
    adlplug
    aether-lv2
    artyFX
    autotalent
    bandithedoge.lv2vst
    bchoppr
    boops
    bristol
    caps
    cardinal
    delayarchitect
    distrho
    dragonfly-reverb
    drumgizmo
    eq10q
    fmit
    geonkick
    guitarix
    gxplugins-lv2
    helm
    infamousPlugins
    lsp-plugins
    mod-distortion
    molot-lite
    mooSpace
    ninjas2
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
