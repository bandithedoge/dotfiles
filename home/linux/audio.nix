{pkgs, home-manager, ... }:
{
  home.packages = with pkgs; [
    milkytracker
    non
    distrho
  ];
}
