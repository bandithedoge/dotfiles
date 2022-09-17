{
  pkgs,
  config,
  ...
}: let
  rice = import ../../../rice.nix {inherit pkgs;};
in {
  home.packages = with pkgs; [xorg.xev];

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

  services.sxhkd = {
    enable = true;
    keybindings = {
      "super + Return" = rice.terminal;
      "super + space" = rice.menu;
      "super + b" = "$BROWSER";
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
      ]) ["kitty"];
  };

  xdg.configFile."awesome/rc.lua" = {
    text = let
      inherit (pkgs.luaPackages) fennel;
      dbus_proxy = pkgs.bandithedoge.luaPackages.lua-dbus_proxy.src;
    in ''
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

  programs.kitty = {
    # {{{
    enable = true;
    font = {
      name = rice.monoFont;
      size = 12;
    };
    keybindings = {
      "ctrl+enter" = "no_op";
      "ctrl+space" = "no_op";
    };
    settings = with rice; {
      term = "xterm-kitty";
      cursor_shape = "beam";
      enable_audio_bell = false;
      disable_ligatures = "cursor";
      window_padding_width = 10;
      adjust_column_width = -1;
      tab_bar_style = "powerline";
      confirm_os_window_close = 0;

      macos_titlebar_color = "background";
      macos_thicken_font = "0.25";

      background = base00;
      foreground = base05;
      selection_background = base05;
      selection_foreground = base00;
      url_color = base0F;
      cursor = base0F;
      active_border_color = base0F;
      inactive_border_color = base01;
      active_tab_background = base0F;
      active_tab_foreground = base00;
      inactive_tab_background = base02;
      inactive_tab_foreground = base05;

      color0 = base01;
      color1 = base08;
      color2 = base0B;
      color3 = base09;
      color4 = base0D;
      color5 = base0E;
      color6 = base0C;
      color7 = base06;

      color8 = base02;
      color9 = base12;
      color10 = base14;
      color11 = base13;
      color12 = base16;
      color13 = base17;
      color14 = base15;
      color15 = base0F;
    };
  };
  # }}}
}
