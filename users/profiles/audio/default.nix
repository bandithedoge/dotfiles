{
  pkgs,
  home-manager,
  ...
}: {
  home.packages = with pkgs; [
    cadence
    qpwgraph
    carla
    ft2-clone
    helvum
    milkytracker
    pt2-clone
    soundtracker
    ardour
  ];
}
