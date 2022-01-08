{pkgs, home-manager, ... }:
{
  home.packages = with pkgs; [
    vcv-rack
    milkytracker
    non
    distrho
  ];
}
