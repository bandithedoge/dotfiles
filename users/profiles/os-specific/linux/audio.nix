{
  pkgs,
  home-manager,
  ...
}: {
  home.packages = with pkgs; [
    ardour
    # bandithedoge.cardinal
    # bandithedoge.zrythm
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
    vcv-rack
    yabridge
    yabridgectl

    # plugins
    ChowKick
    adlplug
    aether-lv2
    artyFX
    autotalent
    bchoppr
    boops
    bristol
    caps
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
