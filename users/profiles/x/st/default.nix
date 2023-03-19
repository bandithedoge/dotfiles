{pkgs}: let
  rice = import ../../../../rice.nix {inherit pkgs;};
in
  pkgs.bandithedoge.st-flexipatch.overrideAttrs (oldAttrs: {
    prePatch = let
      configFile = pkgs.writeText "config.def.h" (with rice; ''
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

        ${builtins.readFile ./config.def.h}
      '');
    in ''
      cp ${configFile} config.def.h
      cp ${./config.mk} config.mk
      cp ${./patches.def.h} patches.def.h
    '';

    buildInputs =
      oldAttrs.buildInputs
      ++ (with pkgs; [
        harfbuzz
        freetype
      ]);
  })
