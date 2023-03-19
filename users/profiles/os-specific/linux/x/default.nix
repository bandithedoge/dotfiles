{pkgs, ...}: let
  rofi-stuff = pkgs.callPackage ../rofi {};
  my-st = pkgs.bandithedoge.st-flexipatch.overrideAttrs (oldAttrs: {
    prePatch = let
      configFile = with pkgs.rice;
        pkgs.writeText "patches.def.h" ''
          static char *font = "${monoFont}:size=11.5:antialias=true:autohint=true";

          static const char *colorname[] = {
            /* 8 normal colors */
            "${base01}",
            "${base08}",
            "${base0B}",
            "${base0A}",
            "${base0D}",
            "${base0E}",
            "${base0C}",
            "${base05}",

            /* 8 bright colors */
            "${base03}",
            "${base12}",
            "${base14}",
            "${base13}",
            "${base16}",
            "${base17}",
            "${base15}",
            "${base0F}",

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
      "super + ctrl + p" = "${rofi-stuff}/bin/keepass";
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

  xdg.configFile = {
    "awesome/rc.lua" = {
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

    "swappy/config".text = pkgs.lib.generators.toINI {} {
      Default = {
        early_exit = true;
      };
    };
  };

  # betterlockscreen {{{
  # systemd.user.services.betterlockscreen = {
  #   Unit = {
  #     Description = "Update betterlockscreen background";
  #     After = ["graphical-session-pre.target"];
  #   };
  #
  #   Install.WantedBy = ["graphical-session.target"];
  #
  #   Service = {
  #     ExecStart = "${pkgs.betterlockscreen}/bin/betterlockscreen -u ${pkgs.rice.wallpaperBlurred}";
  #   };
  # };

  xdg.configFile."betterlockscreenrc".text = let
    color = c: (pkgs.lib.removePrefix "#" c) + "ff";
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
