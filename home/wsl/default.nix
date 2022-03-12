{ pkgs, ... }@inputs:
let
  common = import ../../common { inherit pkgs inputs; };
in
{
  xdg.configFile."nix/nix.conf".text = ''
    ${common.nix.extraOptions}
  '';
}
