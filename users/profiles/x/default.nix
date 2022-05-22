{
  pkgs,
  config,
  ...
}: let
  rice = import ../../../rice.nix;
  my-xmonad = pkgs.callPackage ./xmonad {};
in {
  home.packages = with pkgs; [
    my-xmonad
  ];

  xsession = {
    enable = true;
    windowManager.command = "${my-xmonad}/bin/my-xmonad";
    windowManager.awesome = {
      enable = false;
      luaModules = with pkgs.luaPackages; [
        fennel
        vicious
      ];
    };
    windowManager.xmonad = {
      enable = false;
      config = ./xmonad/xmonad.hs;
      enableContribAndExtras = true;
      extraPackages = hpkgs: with hpkgs; [
        xmobar
      ];
    };
  };

  services.taffybar.enable = false;

  fonts.fontconfig.enable = true;

  xdg.configFile."sx/sxrc" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      exec ${config.xsession.windowManager.command}
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
