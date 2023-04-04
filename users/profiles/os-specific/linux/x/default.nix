{
  pkgs,
  config,
  ...
}: let
  rofi-stuff = pkgs.callPackage ../rofi {};
in {
  home.packages = with pkgs; [
    betterlockscreen
    libnotify
    xclip
    xorg.xev
    xss-lock
  ];

  xsession = {
    enable = true;
    windowManager.awesome = {
      enable = true;
      luaModules = with pkgs.awesome.lua.pkgs; [
        pkgs.bandithedoge.luaPackages.lua-dbus_proxy
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
        "kitty"
      ];
  };

  services.dunst = {
    enable = true;
  };

  systemd.user.services = {
    # xss-lock = {
    #   Unit = {
    #     Description = "xss-lock";
    #     After = ["graphical-session-pre.target"];
    #   };
    #
    #   Install.WantedBy = ["graphical-session.target"];
    #
    #   Service = {
    #     ExecStart = "${pkgs.xss-lock}/bin/xss-lock -- ${pkgs.betterlockscreen} -l";
    #   };
    # };

    betterlockscreen = {
      Unit = {
        Description = "Update betterlockscreen background";
        After = ["graphical-session-pre.target"];
      };

      Install.WantedBy = ["graphical-session.target"];

      Service = {
        ExecStart = "${pkgs.betterlockscreen}/bin/betterlockscreen -u ${pkgs.rice.wallpaperBlurred}";
      };
    };
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

    "sx/sxrc".source = pkgs.writeShellScript "sxrc" ''
      ${config.xsession.windowManager.command}
    '';

    "betterlockscreenrc".text = let
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
  };
}
