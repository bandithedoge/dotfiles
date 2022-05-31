{
  pkgs,
  config,
  ...
}: let
  rice = import ../../../rice.nix;

  my-xmonad = pkgs.callPackage ./xmonad {};

  my-dmenu = pkgs.dmenu.overrideAttrs (oldAttrs: {
    inherit (pkgs.bandithedoge.dmenu-flexipatch) src;

    prePatch = let
      configFile = pkgs.writeText "config.def.h" (with rice; ''
        ${builtins.readFile ./dmenu/config.def.h}

        static const char *fonts[] = {"${uiFont}:size=11"};

        const char *colors[][2] = {
          [SchemeNorm] = {"${base05}", "${base01}"},
          [SchemeSel] = {"${base00}", "${base0F}"},
          [SchemeOut] = {"#000000", "#00ffff"},
          [SchemeBorder] = {"${base00}", "${base0F}"},
          [SchemeSelHighlight] = {"${base0F}", "${base01}"},
          [SchemeNormHighlight] = {"${base0F}", "${base01}"},
        };
      '');
    in ''
      cp ${configFile} config.def.h
      cp ${./dmenu/patches.def.h} patches.def.h
    '';
  });

  my-st = pkgs.st.overrideAttrs (oldAttrs: {
    inherit (pkgs.bandithedoge.st-flexipatch) src;

    prePatch = let
      configFile = pkgs.writeText "config.def.h" (with rice; ''
        ${builtins.readFile ./st/config.def.h}

        static char *font = "${monoFont}:pixelsize=15:autohint=true";

        static const char *colorname[] = {
          "${base01}", "${base08}", "${base0B}", "${base09}", "${base0D}", "${base0E}", "${base0C}", "${base06}",

          "${base02}", "${base12}", "${base14}", "${base13}", "${base16}", "${base17}", "${base15}", "${base0F}",

          [255] = 0,

          "${base0F}", /* 256 -> cursor */
          "${base00}", /* 257 -> rev cursor*/
          "${base00}", /* 258 -> bg */
          "${base05}", /* 259 -> fg */
        };

      '');
    in ''
      cp ${configFile} config.def.h
      cp ${./st/patches.def.h} patches.def.h
      cp ${./st/config.mk} config.mk
    '';

    buildInputs =
      oldAttrs.buildInputs
      ++ (with pkgs; [
        harfbuzz
        freetype
      ]);
  });
in {
  home.packages = with pkgs; [
    my-xmonad
    my-dmenu
    my-st
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
