{
  pkgs,
  config,
  ...
}: let
  rice = import ../../../rice.nix {inherit pkgs;};
in {
  home.packages = with pkgs; [
    (callPackage ./st {})
    (callPackage ./dmenu {})
    j4-dmenu-desktop
    eww
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

  # betterlockscreen {{{
  systemd.user.services.betterlockscreen = {
    Unit = {
      Description = "Update betterlockscreen background";
      After = ["graphical-session-pre.target"];
      PartOf = ["graphical-session.target"];
    };

    Install.WantedBy = ["graphical-session.target"];

    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.betterlockscreen}/bin/betterlockscreen -u ${rice.wallpaperBlurred}";
    };
  };

  xdg.configFile."betterlockscreenrc".text = let
    color = c: (pkgs.lib.removePrefix "#" c) + "ff";
    blank = "00000000";
  in
    with rice; ''
      fx_list=()
      quiet=true

      font="${uiFont}"
      loginbox=${color base00}
      time_format="%a %d %b %T"

      greetercolor=${color base04}
      layoutcolor=${color base03}
      timecolor=${color base05}

      insidewrongcolor=${color base08}
      ringcolor=${color base02}
      ringvercolor=${color base0E}
      ringwrongcolor=${color base08}

      bshlcolor=${color base08}
      keyhlcolor=${color base0F}
      modifcolor=${color base0F}
      verifcolor=${color base0E}
      wrongcolor=${color base08}
    '';
  # }}}

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
