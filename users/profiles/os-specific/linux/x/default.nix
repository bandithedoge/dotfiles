{
  pkgs,
  config,
  ...
}: let
  rofi-stuff = pkgs.callPackage ./rofi {};
  my-st = pkgs.bandithedoge.st-flexipatch.overrideAttrs (oldAttrs: {
    prePatch = let
      configFile = with pkgs.rice;
        pkgs.writeText "patches.def.h" ''
          static char *font = "${monoFont}:size=11.5:antialias=true:autohint=true";

          static const char *colorname[] = {
            /* 8 normal colors */
            "${base01}", "${base08}", "${base0B}", "${base0A}", "${base0D}", "${base0E}", "${base0C}",
            "${base05}",

            /* 8 bright colors */
            "${base03}", "${base12}", "${base14}", "${base13}", "${base16}", "${base17}", "${base15}", "${base0F}",

            [255] = 0,

            "${base0F}", /* 256 -> cursor */
            "${base00}", /* 257 -> rev cursor*/
            "${base00}", /* 258 -> bg */
            "${base05}", /* 259 -> fg */
          };

          ${builtins.readFile ./st/config.h}
        '';
    in ''
      cp ${./st/patches.h} patches.def.h
      cp ${./st/config.mk} config.mk
      cp ${configFile} config.def.h
    '';
    buildInputs = oldAttrs.buildInputs ++ (with pkgs; [harfbuzz]);
  });
in {
  home.packages = with pkgs; [
    my-st
    xclip
    xorg.xev
  ];

  xsession = {
    enable = true;
    windowManager.awesome = {
      enable = true;
      luaModules = with pkgs.luaPackages;
      with pkgs.bandithedoge.luaPackages; [
        fennel
        lua-dbus_proxy
        vicious
        fennel
      ];
    };
  };

  services.sxhkd = {
    enable = true;
    keybindings = {
      "super + Return" = pkgs.rice.terminal;
      "super + space" = pkgs.rice.menu;
      "super + b" = "$BROWSER";
      "super + p" = "strawberry";
      "super + shift + p" = "${rofi-stuff}/bin/keepass";
      "super + d" = "discordcanary";
      "super + Escape" = "pkill -USR1 -x sxhkd";
      "Print" = builtins.toString (pkgs.writeShellScript "screenshot" ''
        sel=$(${pkgs.hacksaw}/bin/hacksaw -f "-i %i -g %g" -c "${pkgs.rice.base0F}" -g 2 -s 2)
        ${pkgs.shotgun}/bin/shotgun $sel - | ${pkgs.swappy}/bin/swappy -f -
      '');

      "XF86AudioMute" = "amixer set Master toggle";
      "XF86AudioRaiseVolume" = "amixer set Master 5%+";
      "XF86AudioLowerVolume" = "amixer set Master 5%-";
      "XF86AudioPlay" = "playerctl play-pause";
      "XF86AudioPrev" = "playerctl previous";
      "XF86AudioNext" = "playerctl next";
    };
  };

  fonts.fontconfig.enable = true;

  services.picom = {
    enable = true;
    fade = true;
    fadeDelta = 5;
    shadow = true;
    shadowExclude = [
      "name = 'hacksaw'"
    ];
    opacityRules =
      builtins.concatMap
      (class: [
        "95:class_g = '${class}' && focused"
        "85:class_g = '${class}' && !focused"
      ]) [
        "st-256color"
      ];
  };

  xdg.configFile."awesome/rc.lua" = {
    text = ''
      local fennel = require("fennel")
      debug.traceback = fennel.traceback
      fennel.path = fennel.path .. ";${./awesome}/?.fnl"
      table.insert(package.loaders or package.searchers, fennel.searcher)

      ${pkgs.rice.def.lua}

      require "config"
    '';
    onChange = ''
      awesome-client "awesome.restart()"
    '';
  };

  xdg.configFile."swappy/config".text = pkgs.lib.generators.toINI {} {
    Default = {
      early_exit = true;
    };
  };

  # betterlockscreen {{{
  systemd.user.services.betterlockscreen = {
    Unit = {
      Description = "Update betterlockscreen background";
      After = ["graphical-session-pre.target"];
    };

    Install.WantedBy = ["graphical-session.target"];

    Service = {
      Type = "simple";
      ExecStart = "${pkgs.betterlockscreen}/bin/betterlockscreen -u ${pkgs.rice.wallpaperBlurred}";
    };
  };

  xdg.configFile."betterlockscreenrc".text = let
    color = c: (pkgs.lib.removePrefix "#" c) + "ff";
    blank = "00000000";
  in
    with pkgs.rice; ''
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

  # polkit {{{
  systemd.user.services.polkit-gnome-authentication-agent-1 = {
    Unit = {
      Description = "polkit-gnome-authentication-agent-1";
      After = ["graphical-session.target"];
    };

    Install = {
      WantedBy = ["graphical-session.target"];
      Wants = ["graphical-session.target"];
      After = ["graphical-session.target"];
    };

    Service = {
      Type = "simple";
      ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
      Restart = "on-failure";
      RestartSec = 1;
      TimeoutStopSec = 10;
    };
  };
  # }}}

  programs.rofi = {
    # {{{
    enable = true;
    package = pkgs.rofi-wayland;
    font = "${pkgs.rice.uiFont} 12";
    plugins = with pkgs; [rofi-calc];
    extraConfig = {
      modi = "power:${rofi-stuff}/bin/power,drun,run,calc";

      drun-match-fields = "name,exec";
      drun-display-format = "{name}";
      show-icons = true;
      scroll-method = 1;

      kb-mode-complete = "";
      kb-remove-to-eol = "";
      kb-remove-to-sol = "";
      kb-remove-char-forward = "Delete";
      kb-row-up = "Control+k";
      kb-row-down = "Control+j";
      kb-mode-next = "Control+l";
      kb-mode-previous = "Control+h";
      kb-page-prev = "Control+u";
      kb-page-next = "Control+d";
      kb-accept-entry = "Return";
      kb-remove-char-back = "BackSpace";
      kb-screenshot = "Control+Shift+s";
    };
    theme = with pkgs.rice; let
      inherit (config.lib.formats.rasi) mkLiteral;
      padding = mkLiteral "5px";
    in {
      "*" = {
        background-color = mkLiteral base10;
        text-color = mkLiteral base05;
      };

      window = {
        border = mkLiteral "2px";
        children = map mkLiteral ["mainbox"];
        border-color = mkLiteral base0F;
      };

      mainbox = {
        children = map mkLiteral ["inputbar" "message" "listview" "mode-switcher"];
        spacing = padding;
        margin = padding;
      };

      inputbar = {
        children = map mkLiteral ["prompt" "entry"];
        spacing = mkLiteral "0";
      };

      prompt = {
        background-color = mkLiteral base0F;
        text-color = mkLiteral base00;
        vertical-align = mkLiteral "0.5";
        horizontal-align = mkLiteral "0.5";
        inherit padding;
      };

      entry = {
        background-color = mkLiteral base00;
        vertical-align = mkLiteral "0.5";
        inherit padding;
      };

      listview = {
        background-color = mkLiteral base00;
        scrollbar = true;
        spacing = mkLiteral "0px 5px";
        inherit padding;
      };

      element = {
        children = map mkLiteral ["element-icon" "element-text"];
        background-color = mkLiteral "transparent";
        padding = mkLiteral "2px";
        spacing = padding;
      };
      element-icon = {
        background-color = mkLiteral "inherit";
        size = mkLiteral "1.25em";
      };
      element-text = {
        background-color = mkLiteral "inherit";
        highlight = mkLiteral "bold ${base0F}";
        vertical-align = mkLiteral "0.5";
      };

      "element.selected.normal".background-color = mkLiteral base02;
      "element.urgent".text-color = mkLiteral base08;
      "element.active".text-color = mkLiteral base0B;
      "element.selected.urgent".background-color = mkLiteral base08;
      "element.selected.active".background-color = mkLiteral base0B;

      scrollbar = {
        background-color = mkLiteral base00;
        handle-color = mkLiteral base0F;
        handle-width = mkLiteral "5px";
      };

      button = {
        background-color = mkLiteral base01;
        text-color = mkLiteral base03;
      };
      "button.selected" = {
        background-color = mkLiteral base0F;
        text-color = mkLiteral base00;
      };
    };
  }; # }}}

  services.flameshot = {
    enable = true;
    settings = {
      General = {
        uiColor = pkgs.rice.base0F;
        disabledTrayIcon = true;
        showSidePanelButton = false;
        showDesktopNotification = false;
        autoCloseIdleDaemon = true;
      };
    };
  };
}
