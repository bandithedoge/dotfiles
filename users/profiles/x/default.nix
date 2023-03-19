{
  pkgs,
  config,
  ...
}: let
  rice = import ../../../rice.nix;

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
    my-st
  ];

  xsession = {
    enable = true;
    windowManager.awesome = {
      enable = true;
      package = pkgs.awesome.override {lua = pkgs.luajit;};
      luaModules = with pkgs.luaPackages; [
        vicious
      ];
    };
  };

  fonts.fontconfig.enable = true;

  services.picom = {
    enable = true;
    fade = true;
    fadeDelta = 5;
    shadow = true;
    shadowExclude = ["class_g = 'awesome'"];
    opacityRules = builtins.concatMap (class: [
      "95:class_g = '${class}' && focused"
      "85:class_g = '${class}' && !focused"
    ]) ["st-256color"];
  };

  xdg.configFile."sx/sxrc" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      exec ${config.xsession.windowManager.command}
    '';
  };

  xdg.configFile."awesome/rc.lua" = {
    text = ''
      package.path = package.path .. ";${pkgs.luaPackages.fennel}/?.lua"

      local fennel = require("fennel")
      debug.traceback = fennel.traceback
      fennel.path = fennel.path .. ";${./awesome}/?.fnl"
      table.insert(package.loaders or package.searchers, fennel.searcher)

      ${rice.def.lua}

      require "config"
    '';
    onChange = ''
      awesome-client "awesome.restart()"
    '';
  };

  programs.rofi = {
    # {{{
    enable = true;
    package = pkgs.rofi-wayland;
    font = rice.uiFont + " 12";
    extraConfig = {
      modi = "power:${./bin/rofi/power.lua},drun,run";
      drun-match-fields = "name,exec";
      drun-display-format = "{name}";
      show-icons = true;
      scroll-method = 1;
      kb-row-up = "Control+k";
      kb-row-down = "Control+j";
      kb-mode-next = "Control+l";
      kb-mode-previous = "Control+h";
      kb-mode-complete = "";
      kb-remove-to-eol = "";
      kb-accept-entry = "Return";
      kb-remove-char-back = "BackSpace";
    };
    theme = with rice; let
      inherit (config.lib.formats.rasi) mkLiteral;
      padding = mkLiteral "5px";
    in {
      "*" = {
        border-color = mkLiteral base0F;
        background-color = mkLiteral base00;
        text-color = mkLiteral base05;
      };

      mainbox.children =
        map mkLiteral ["inputbar" "message" "mode-switcher" "listview"];

      window = {
        border = mkLiteral "2px";
      };

      entry = {inherit padding;};

      prompt = {
        inherit padding;
        background-color = mkLiteral base0F;
        text-color = mkLiteral base00;
      };

      listview.scrollbar = true;

      scrollbar.handle-color = mkLiteral base0F;

      element = {
        background-color = mkLiteral "transparent";
        padding = mkLiteral "2px 5px";
      };
      element-icon = {
        background-color = mkLiteral "inherit";
        padding = mkLiteral "0 5px";
      };
      "element.selected.normal".background-color = mkLiteral base02;
      element-text = {
        background-color = mkLiteral "inherit";
        highlight = mkLiteral "bold ${base0F}";
      };

      "element.urgent".text-color = mkLiteral base08;
      "element.active".text-color = mkLiteral base0B;
      "element.selected.urgent".background-color = mkLiteral base08;
      "element.selected.active".background-color = mkLiteral base0B;

      button.text-color = mkLiteral base03;
      "button.selected".text-color = mkLiteral base05;
    };
  }; # }}}
}
