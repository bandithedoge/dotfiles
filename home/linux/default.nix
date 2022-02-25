{ config, pkgs, ... }:
let
  rice = import ../../rice.nix;
in
{
  imports = [ ./audio.nix ];
  home.packages = with pkgs; [
    icon-library
    imv
    pavucontrol
    (river.override { xwaylandSupport = false; })
    swaylock-effects
    tigervnc
    wl-clipboard
    wlr-randr
    yambar
  ];

  # river {{{
  xdg.configFile."river/init" =
    let colors = color: "0x" + pkgs.lib.strings.removePrefix "#" color;
    in
    {
      executable = true;
      text = with rice; ''
        #!/usr/bin/env bash

        mod="Mod4"

        riverctl map normal $mod Return spawn ${terminal}
        riverctl map normal $mod Space spawn "${menu}"
        riverctl map normal $mod W close
        riverctl map normal $mod+Control Q exit

        riverctl map normal $mod J focus-view next
        riverctl map normal $mod K focus-view previous
        riverctl map normal $mod+Shift J swap next
        riverctl map normal $mod+Shift K swap previous

        for i in $(seq 1 9)
        do
          tags=$((1 << ($i - 1)))

          riverctl map normal $mod $i set-focused-tags $tags
          riverctl map normal $mod+Shift $i set-view-tags $tags
          riverctl map normal $mod+Control $i toggle-focused-tags $tags
          riverctl map normal $mod+Shift+Control $i toggle-view-tags $tags
        done

        riverctl map normal $mod H send-layout-cmd rivertile "main-ratio -0.03"
        riverctl map normal $mod L send-layout-cmd rivertile "main-ratio +0.03"
        riverctl map normal $mod+Shift H send-layout-cmd rivertile "main-count +1"
        riverctl map normal $mod+Shift L send-layout-cmd rivertile "main-count -1"

        riverctl map normal $mod+Control H send-layout-cmd rivertile "main-location left"
        riverctl map normal $mod+Control J send-layout-cmd rivertile "main-location bottom"
        riverctl map normal $mod+Control K send-layout-cmd rivertile "main-location top"
        riverctl map normal $mod+Control L send-layout-cmd rivertile "main-location right"

        riverctl map-pointer normal $mod BTN_LEFT move-view
        riverctl map-pointer normal $mod BTN_RIGHT resize-view

        riverctl map normal $mod F toggle-fullscreen
        riverctl map normal $mod T toggle-float

        riverctl float-filter-add app-id float

        riverctl background-color ${colors base00}
        riverctl border-color-focused ${colors base0F}
        riverctl border-color-unfocused ${colors base00}
        riverctl focus-follows-cursor normal

        riverctl default-layout rivertile
        exec yambar &
        exec rivertile -view-padding 5 -outer-padding 5 -main-ratio 0.5
      '';
    };
  # }}}

  # yambar {{{
  xdg.configFile."yambar/config.yml".text = with rice;
    let
      color = hex: pkgs.lib.strings.removePrefix "#" hex + "ff";
      spacing = 5;
      module = text: {
        inherit text;
        margin = spacing;
        deco.background.color = color base02;
      };
      icon = text:
        module text // {
          font = rice.monoFont + ":size=12";
          deco.background.color = color base01;
        };
      module_red = text: (module text) // { foreground = color base08; };
      bitrate = [
        { string = icon ""; }
        { string = module "{rx-bitrate} Mb/s"; }
        { string = icon "祝"; }
        { string = module "{tx-bitrate} Mb/s"; }
      ];
    in
    builtins.toJSON {
      bar = {
        location = "top";
        height = 24;
        inherit spacing;
        background = color base01;
        foreground = color base05;
        font = uiFont + ":size=14";
        left = [
          {
            "river" = {
              content.map =
                let
                  tagmap = {
                    tag = "focused";
                    values = {
                      true.string = {
                        text = "{id}";
                        deco.background.color = color base0F;
                        foreground = color base00;
                        font = monoFont;
                        margin = spacing;
                      };
                      false.string = {
                        text = "{id}";
                        deco.background.color = color base02;
                        foreground = color base03;
                        font = monoFont;
                        margin = spacing;
                      };
                    };
                  };
                in
                {
                  tag = "occupied";
                  values.false.map = {
                    tag = "focused";
                    values = {
                      inherit (tagmap.values) true;
                      false.empty = { };
                    };
                  };
                  values.true.map = tagmap;
                };
            };
          }
          {
            "river" = {
              content.empty = { };
              title.string.text = "{title}";
            };
          }
        ];
        right = [
          {
            "network" = {
              name = "wlp3s0";
              content.map = {
                tag = "state";
                values = {
                  down = [
                    { empty = { }; }
                  ];
                  up = [
                    { string = icon "直"; }
                    { string = module "{ssid}"; }
                  ];
                };
              };
            };
          }
          {
            "network" = {
              name = "enp0s25";
              content.map = {
                tag = "state";
                values = {
                  down = [
                    { empty = { }; }
                  ];
                  up = [
                    { string = icon ""; }
                    { string = module "{name}"; }
                  ];
                };
              };
            };
          }
          {
            "battery" = {
              name = "BAT0";
              content.map = {
                tag = "state";
                values = {
                  full = [{ string = icon ""; } { string = module "Full"; }];
                  charging = [
                    { string = icon ""; }
                    { string = module "{capacity}%"; }
                  ];
                  discharging = [
                    { string = icon ""; }
                    { string = module "{capacity}%"; }
                  ];
                  unknown = [
                    { string = icon ""; }
                    { string = module "{capacity}%"; }
                  ];
                };
              };
            };
          }
          {
            "alsa" = {
              card = "default";
              mixer = "Master";
              content.map = {
                on-click = "${pkgs.pavucontrol}/bin/pavucontrol";
                tag = "muted";
                values = {
                  false = [
                    { string = icon "墳"; }
                    { string = module "{percent}%"; }
                  ];
                  true =
                    [{ string = icon "婢"; } { string = module_red "Muted"; }];
                };
              };
            };
          }
          {
            "clock" = {
              date-format = "%A %d %B";
              content =
                [{ string = icon ""; } { string = module "{date}"; }];
            };
          }
          {
            "clock" = {
              time-format = "%H:%M:%S";
              content =
                [{ string = icon ""; } { string = module "{time}"; }];
            };
          }
        ];
      };
    };
  # }}}

  # swaylock
  xdg.configFile."swaylock/config".text =
    let
      color = hex: pkgs.lib.strings.removePrefix "#" hex;
    in
    with rice;
    ''
      ignore-empty-password
      show-failed-attempts
      grace=5

      fade-in=0.3
      indicator
      clock
      screenshots
      effect-blur=10x1

      font=${uiFont}
      color=${color base00}
      inside-color=${color base02}
      inside-clear-color=${color base0C}
      inside-caps-lock-color=${color base0E}
      inside-ver-color=${color base0A}
      inside-wrong-color=${color base08}
      key-hl-color=${color base0F}
    '';

  gtk = {
    # {{{
    enable = true;
    font = {
      name = rice.uiFont;
      size = 12;
    };
    iconTheme = {
      package = pkgs.numix-icon-theme;
      name = "Numix";
    };
    theme = {
      package = pkgs.materia-theme.override { };
      name = "Materia-dark";
    };
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
      gtk-enable-primary-paste = false;
    };
  }; # }}}

  programs.rofi = {
    # {{{
    enable = true;
    package = pkgs.nur.repos.kira-bruneau.rofi-wayland;
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
    theme = with rice;
      let
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
          anchor = mkLiteral "north";
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

  programs.mako = {
    # {{{
    enable = true;
  }; # }}}

  programs.qutebrowser = {
    # {{{
    enable = true;
    package = if pkgs.stdenv.isDarwin then pkgs.dummy else pkgs.qutebrowser;
    searchEngines = {
      DEFAULT = "https://searx.be/search?q={}";
      g = "https://www.google.com/search?q={}";
      a = "https://wiki.archlinux.org/?search={}";
      n = "https://nixos.wiki/index.php?search={}";
    };
    settings = {
      scrolling.smooth = true;
      fonts = {
        default_family = rice.uiFont;
        default_size = "14px";
      };
      colors = with rice; {
        completion.fg = base05;
        completion.odd.bg = base01;
        completion.even.bg = base00;
        completion.category.fg = base0A;
        completion.category.bg = base00;
        completion.category.border.top = base00;
        completion.category.border.bottom = base00;
        completion.item.selected.fg = base05;
        completion.item.selected.bg = base02;
        completion.item.selected.border.top = base02;
        completion.item.selected.border.bottom = base02;
        completion.item.selected.match.fg = base0B;
        completion.match.fg = base0B;
        completion.scrollbar.fg = base05;
        completion.scrollbar.bg = base00;
        contextmenu.disabled.bg = base01;
        contextmenu.disabled.fg = base04;
        contextmenu.menu.bg = base00;
        contextmenu.menu.fg = base05;
        contextmenu.selected.bg = base02;
        contextmenu.selected.fg = base05;
        downloads.bar.bg = base00;
        downloads.start.fg = base00;
        downloads.start.bg = base0D;
        downloads.stop.fg = base00;
        downloads.stop.bg = base0C;
        downloads.error.fg = base08;
        hints.fg = base00;
        hints.bg = base0A;
        hints.match.fg = base05;
        keyhint.fg = base05;
        keyhint.suffix.fg = base05;
        keyhint.bg = base00;
        messages.error.fg = base00;
        messages.error.bg = base08;
        messages.error.border = base08;
        messages.warning.fg = base00;
        messages.warning.bg = base0E;
        messages.warning.border = base0E;
        messages.info.fg = base05;
        messages.info.bg = base00;
        messages.info.border = base00;
        prompts.fg = base05;
        prompts.border = base00;
        prompts.bg = base00;
        prompts.selected.bg = base02;
        prompts.selected.fg = base05;
        statusbar.normal.fg = base0B;
        statusbar.normal.bg = base00;
        statusbar.insert.fg = base00;
        statusbar.insert.bg = base0D;
        statusbar.passthrough.fg = base00;
        statusbar.passthrough.bg = base0C;
        statusbar.private.fg = base00;
        statusbar.private.bg = base01;
        statusbar.command.fg = base05;
        statusbar.command.bg = base00;
        statusbar.command.private.fg = base05;
        statusbar.command.private.bg = base00;
        statusbar.caret.fg = base00;
        statusbar.caret.bg = base0E;
        statusbar.caret.selection.fg = base00;
        statusbar.caret.selection.bg = base0D;
        statusbar.progress.bg = base0D;
        statusbar.url.fg = base05;
        statusbar.url.error.fg = base08;
        statusbar.url.hover.fg = base05;
        statusbar.url.success.http.fg = base0C;
        statusbar.url.success.https.fg = base0B;
        statusbar.url.warn.fg = base0E;
        tabs.bar.bg = base00;
        tabs.indicator.start = base0D;
        tabs.indicator.stop = base0C;
        tabs.indicator.error = base08;
        tabs.odd.fg = base05;
        tabs.odd.bg = base01;
        tabs.even.fg = base05;
        tabs.even.bg = base00;
        tabs.pinned.even.bg = base0C;
        tabs.pinned.even.fg = base07;
        tabs.pinned.odd.bg = base0B;
        tabs.pinned.odd.fg = base07;
        tabs.pinned.selected.even.bg = base02;
        tabs.pinned.selected.even.fg = base05;
        tabs.pinned.selected.odd.bg = base02;
        tabs.pinned.selected.odd.fg = base05;
        tabs.selected.odd.fg = base05;
        tabs.selected.odd.bg = base02;
        tabs.selected.even.fg = base05;
        tabs.selected.even.bg = base02;
        webpage.bg = base00;
      };
    };
  };
  # }}}

  programs.firefox = {
    # {{{
    enable = true;
    package =
      if pkgs.stdenv.isDarwin then pkgs.dummy else pkgs.firefox-unwrapped;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      auto-tab-discard
      betterttv
      buster-captcha-solver
      canvasblocker
      clearurls
      close-other-windows
      facebook-container
      gesturefy
      greasemonkey
      h264ify
      honey
      https-everywhere
      i-dont-care-about-cookies
      netflix-1080p
      octolinker
      octotree
      old-reddit-redirect
      polish-dictionary
      privacy-possum
      reddit-enhancement-suite
      refined-github
      sponsorblock
      stylus
      tabcenter-reborn
      terms-of-service-didnt-read
      translate-web-pages
      tridactyl
      ublock-origin
      unpaywall
      view-image
      violentmonkey
    ];
    profiles."main" = {
      name = "main";
      settings = {
        "app.update.auto" = false;
        "browser.autofocus" = false;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "layers.acceleration.force-enabled" = true;
        "gfx.webrender.all" = true;
        "svg.context-properties.content.enabled" = true;
      };
      userChrome = builtins.readFile (builtins.fetchurl
        "https://raw.githubusercontent.com/ranmaru22/firefox-vertical-tabs/main/userChrome.css");
    };
  };
  # }}}

  programs.kitty = {
    # {{{
    enable = true;
    package = if pkgs.stdenv.isDarwin then pkgs.dummy else pkgs.kitty;
    font = {
      name = rice.monoFont;
      size = if pkgs.stdenv.isDarwin then 16 else 12;
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

  programs.mpv = {
    # {{{
    enable = true;
    scripts = with pkgs.mpvScripts; [
      cutter
      thumbnail
      sponsorblock
      youtube-quality
    ];
  };
  # }}}

  programs.zathura = {
    # {{{
    enable = true;
    options = with rice; {
      page-padding = 10;
      show-hidden = true;
      font = uiFont + " 12";
      recolor = true;

      default-bg = base00;
      default-fg = base01;
      statusbar-fg = base04;
      statusbar-bg = base02;
      inputbar-bg = base00;
      inputbar-fg = base07;
      notification-bg = base00;
      notification-fg = base07;
      notification-error-bg = base00;
      notification-error-fg = base08;
      notification-warning-bg = base00;
      notification-warning-fg = base09;
      highlight-color = base0A;
      highlight-active-color = base0D;
      completion-bg = base01;
      completion-fg = base0D;
      completion-highlight-fg = base0F;
      completion-highlight-bg = base02;
      recolor-lightcolor = base00;
      recolor-darkcolor = base06;
    };
  };
  # }}}

  services.syncthing.enable = true;

  services.swayidle =
    let
      swaylock = "${pkgs.swaylock-effects}/bin/swaylock";
    in
    {
      enable = true;
      timeouts = [{
        timeout = 300;
        command = swaylock;
      }];
      events = [{
        event = "before-sleep";
        command = swaylock;
      }];
    };
}
