{pkgs, home-manager, ... }:
{
  home.packages = with pkgs; [
    cadence
    qpwgraph
    carla
    distrho
    ft2-clone
    helvum
    milkytracker
    non
    pt2-clone
    soundtracker
    ardour
  ];
}
