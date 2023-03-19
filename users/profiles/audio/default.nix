{
  pkgs,
  home-manager,
  ...
}: {
  home.packages = with pkgs; [
    ardour
    breeze-icons
    cardinal
    vcv-rack
    cadence
    carla
    ft2-clone
    helvum
    milkytracker
    pt2-clone
    qpwgraph
    soundtracker
    zrythm
  ];
}
