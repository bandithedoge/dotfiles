{
  pkgs,
  config,
  ...
}: let
  rice = import ../../../rice.nix;

  my-xmonad = pkgs.callPackage ./xmonad {
    inherit (pkgs.bandithedoge) taffybar xmonad-entryhelper;
  };

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
    # my-xmonad
    # my-st
    # haskellPackages.status-notifier-item
  ];

  xsession = {
    enable = true;
    windowManager.command = "${my-xmonad}/bin/my-xmonad";
  };

  fonts.fontconfig.enable = true;

  xdg.configFile."sx/sxrc" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      dbus-launch --auto-syntax

      status-notifier-watcher &

      exec ${config.xsession.windowManager.command}
    '';
  };

  xdg.configFile."taffybar/taffybar.css".text = with rice; ''
    ${def.gtk}

    .taffy-window * {
      all: unset;
      font-family: "${uiFont}";
      font-size: 11pt;
      color: @base05;
    }

    .workspace-label {
      font-family: "${monoFont}";
    }

    ${builtins.readFile ./xmonad/taffybar/taffybar.css}
  '';
}
