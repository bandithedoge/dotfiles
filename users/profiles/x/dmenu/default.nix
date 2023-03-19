{pkgs}: let
  rice = import ../../../../rice.nix {inherit pkgs;};
in
  pkgs.bandithedoge.dmenu-flexipatch.overrideAttrs (oldAttrs: {
    prePatch = let
      configFile = pkgs.writeText "config.def.h" (with rice; ''
        static const char *colors[][2] = {
          [SchemeNorm] = {"${base05}", "${base02}"},
          [SchemeSel] = {"${base00}", "${base0F}"},
          [SchemeOut] = {"#000000", "#00ffff"},
          [SchemeBorder] = {"${base00}", "${base0F}"},
          [SchemeSelHighlight] = {"${base0F}", "${base00}"},
          [SchemeNormHighlight] = {"${base0F}", "${base02}"},
        };

        static const char *fonts[] = {"${uiFont}:size=12"};

        ${builtins.readFile ./config.def.h}
      '');
    in ''
      cp ${configFile} config.def.h
      cp ${./config.mk} config.mk
      cp ${./patches.def.h} patches.def.h
    '';

    nativeBuildInputs = with pkgs; [
      pkgconfig
    ];

    buildInputs =
      oldAttrs.buildInputs
      ++ (with pkgs; [
        pango
      ]);
  })
