{ pkgs, config, ... }:
let
  rice = import ../../rice.nix;
in
{
  home.packages = with pkgs; [
  ];

  xsession = {
    enable = true;
    windowManager.awesome = {
      enable = true;
      luaModules = with pkgs.luaPackages; [
        fennel
        vicious
      ];
    };
  };

  xdg.configFile."sx/sxrc" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      ${config.xsession.windowManager.command}
    '';
  };

  xdg.configFile."awesome/rc.lua".text = ''
    package.path = package.path .. ";${pkgs.luaPackages.fennel}/?.lua"

    local fennel = require("fennel")
    debug.traceback = fennel.traceback
    fennel.path = fennel.path .. ";${./awesome}/?.fnl"
    table.insert(package.loaders or package.searchers, fennel.searcher)

    ${rice.def.lua}

    require "config"
  '';
}
