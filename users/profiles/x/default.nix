{ pkgs
, config
, ...
}:
let
  rice = import ../../../rice.nix { inherit pkgs; };
  my-st = pkgs.bandithedoge.st-flexipatch.overrideAttrs (oldAttrs: {
    prePatch =
      let
        configFile = with rice;
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
      in
      ''
        cp ${./st/patches.h} patches.def.h
        cp ${./st/config.mk} config.mk
        cp ${configFile} config.def.h
      '';
    buildInputs = oldAttrs.buildInputs ++ (with pkgs; [ harfbuzz ]);
  });
in
{
  home.packages = with pkgs; [ xorg.xev my-st ];

  xsession = {
    enable = true;
    windowManager.awesome = {
      enable = true;
      package = pkgs.awesome.override { lua = pkgs.luajit; };
      luaModules = with pkgs.luaPackages; [
        vicious
      ];
    };
  };

  services.sxhkd = {
    enable = true;
    keybindings = {
      "super + Return" = rice.terminal;
      "super + space" = rice.menu;
      "super + b" = "$BROWSER";
      "super + p" = "strawberry";
      "super + d" = "discordcanary";
      "super + Escape" = "pkill -USR1 -x sxhkd";
      "Print" = "flameshot gui";

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
    opacityRules =
      builtins.concatMap
        (class: [
          "95:class_g = '${class}' && focused"
          "85:class_g = '${class}' && !focused"
        ]) [ "st-256color" ];
  };

  xdg.configFile."awesome/rc.lua" = {
    text =
      let
        inherit (pkgs.luaPackages) fennel;
        dbus_proxy = pkgs.bandithedoge.luaPackages.lua-dbus_proxy.src;
      in
      ''
        package.path = package.path .. ";${fennel}/?.lua;${dbus_proxy}/src/?/init.lua;${dbus_proxy}/src/?.lua;"

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
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Install.WantedBy = [ "graphical-session.target" ];

    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.betterlockscreen}/bin/betterlockscreen -u ${rice.wallpaperBlurred}";
    };
  };

  xdg.configFile."betterlockscreenrc".text =
    let
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
    in
    {
      "*" = {
        border-color = mkLiteral base0F;
        background-color = mkLiteral base00;
        text-color = mkLiteral base05;
      };

      mainbox.children =
        map mkLiteral [ "inputbar" "message" "mode-switcher" "listview" ];

      window = {
        border = mkLiteral "2px";
      };

      entry = { inherit padding; };

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

  services.flameshot = {
    enable = true;
    settings = {
      General = {
        uiColor = rice.base0F;
        disabledTrayIcon = true;
        showSidePanelButton = false;
        showDesktopNotification = false;
        autoCloseIdleDaemon = true;
      };
    };
  };
}
