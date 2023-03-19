{
  pkgs,
  home-manager,
  ...
}: {
  home.packages = with pkgs; [
    cadence
    carla
    ft2-clone
    helvum
    milkytracker
    pt2-clone
    qpwgraph
    soundtracker
    volctl
    mate.mate-media
  ];
}
