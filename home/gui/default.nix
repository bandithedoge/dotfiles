{
  pkgs,
  config,
  ...
}: {
  programs.kitty = {
    # TODO {{{
    enable = true;
    package =
      if pkgs.stdenv.isDarwin
      then pkgs.dummy
      else pkgs.kitty;
    font = {
      name = pkgs.rice.monoFont;
      size =
        if pkgs.stdenv.isDarwin
        then 16
        else 12;
    };
    keybindings = {
      "ctrl+enter" = "no_op";
      "ctrl+space" = "no_op";
    };
    settings = with pkgs.rice; {
      term = "xterm-kitty";
      cursor_shape = "beam";
      enable_audio_bell = false;
      disable_ligatures = "cursor";
      window_padding_width = 5;
      adjust_column_width = -1;
      tab_bar_style = "powerline";
      confirm_os_window_close = 0;
      shell = "${pkgs.fish}/bin/fish";
      sync_to_monitor = false;
      macos_titlebar_color = "background";
      text_composition_strategy = "legacy";

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

  programs.alacritty = {
    # {{{
    enable = true;
    settings = {
      shell = "zellij";
      window = {
        padding = {
          x = 5;
          y = 5;
        };
        dynamic_padding = true;
        class.general = "alacritty";
      };
      font = {
        normal.family = pkgs.rice.monoFont;
        size = 11.5;
      };
      colors = with pkgs.rice; {
        primary = {
          foreground = base05;
          background = base00;
        };
        cursor = {
          text = base00;
          cursor = base0F;
        };
        normal = {
          black = base01;
          red = base08;
          green = base0B;
          yellow = base0A;
          blue = base0D;
          magenta = base0E;
          cyan = base0C;
        };
        bright = {
          black = base03;
          red = base12;
          green = base14;
          yellow = base13;
          blue = base16;
          magenta = base17;
          cyan = base15;
          white = base0F;
        };
      };
      bell = {
        animation = "EaseOutQuad";
        duration = 500;
        color = pkgs.rice.base0F;
      };
      cursor = {
        style = "Beam";
        thickness = 0.25;
      };
      terminal.osc52 = "CopyPaste";
      mouse.hide_when_typing = true;
    };
  }; # }}}

  programs.firefox = {
    # {{{
    enable = config.hostname == "machine-nixos";
    package =
      if !pkgs.stdenv.isDarwin
      then
        pkgs.firefox_nightly.override
        {
          cfg = {
            nativeMessagingHosts = with pkgs; [tridactyl-native];
          };
        }
      else pkgs.dummy;
    profiles = {
      default = {
        name = "default";
        extensions = with pkgs.bandithedoge.firefoxAddons; [
          augmented-steam
          auto-tab-discard
          base64-decoder
          betterviewer
          csgofloat
          dont-fuck-with-paste
          downthemall
          enhanced-github
          gesturefy
          gitako
          github-code-folding
          github-isometric-contributions
          github-repo-size
          imagus
          lovely-forks
          material-icons-for-github
          npm-hub
          octolinker
          privacy-badger
          privacy-pass
          pronoundb
          reddit-enhancement-suite
          refined-github
          sidebery
          sourcegraph
          sponsorblock
          steam-database
          stylus
          tridactyl
          ublock-origin
          violentmonkey
        ];
        settings = {
          "accessibility.force_disabled" = 1;
          "app.update.auto" = false;
          "browser.autofocus" = false;
          "browser.compactmode.show" = true;
          "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" = false;
          "browser.startup.page" = 3;
          "browser.urlbar.addons.featureGate" = false;
          "browser.urlbar.suggest.engines" = false;
          "browser.urlbar.suggest.topsites" = false;
          "browser.urlbar.suggest.trending" = false;
          "browser.urlbar.suggest.weather" = false;
          "browser.urlbar.trimHttps" = true;
          "browser.vpn_promo.enabled" = false;
          "devtools.accessibility.enabled" = false;
          "devtools.chrome.enabled" = true;
          "devtools.debugger.ui.editor-wrapping" = true;
          "extensions.update.enabled" = false;
          "general.autoScroll" = true;
          "gfx.webrender.all" = true;
          "gfx.webrender.quality.force-subpixel-aa-where-possible" = true;
          "image.jxl.enabled" = true;
          "layers.acceleration.force-enabled" = true;
          "layout.css.has-selector.enabled" = true;
          "layout.css.moz-document.content.enabled" = true;
          "layout.css.prefers-color-scheme.content-override" = 0;
          "pdfjs.sidebarViewOnLoad" = 2;
          "signon.generation.enabled" = false;
          "svg.context-properties.content.enabled" = true;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "ui.context_menus.after_mouseup" = true;
          "ui.key.menuAccessKeyFocuses" = false;
          "view_source.wrap_long_lines" = true;
          "widget.non-native-theme.use-theme-accent" = true;

          "userChrome.autohide.back_button" = true;
          "userChrome.autohide.bookmarkbar" = true;
          "userChrome.autohide.forward_button" = true;
          "userChrome.autohide.page_action" = true;
          "userChrome.autohide.sidebar" = true;
          "userChrome.autohide.toolbar_overlap" = true;
          "userChrome.centered.urlbar" = true;
          "userChrome.hidden.sidebar_header" = true;
          "userChrome.hidden.tabbar" = true;
          "userChrome.icon.panel_full" = true;
          "userChrome.padding.bookmarkbar" = true;
          "userChrome.padding.menu" = true;
          "userChrome.padding.menu_compact" = true;
          "userChrome.padding.panel" = true;
          "userChrome.padding.panel_header" = true;
          "userChrome.padding.urlbar" = true;
          "userChrome.panel.remove_strip" = true;
          "userChrome.sidebar.overlap" = true;
          "userChrome.urlView.go_button_when_typing" = true;
          "userChrome.urlView.move_icon_to_left" = true;
          "userContent.page.dark_mode" = true;
          "userContent.page.illustration" = true;
          "userContent.page.proton" = true;
          "userContent.page.proton_color" = true;
          "userContent.page.proton_color.system_accent" = true;
          "userContent.player.animate" = true;
          "userContent.player.icon" = true;
          "userContent.player.size" = true;
          "userContent.player.ui" = true;
        };
        extraConfig =
          builtins.readFile (builtins.fetchurl "https://raw.githubusercontent.com/yokoffing/Betterfox/main/user.js")
          + ''
            user_pref("identity.fxaccounts.enabled", true);
          '';
        userChrome =
          builtins.readFile (pkgs.rice.compileSCSS ./firefox.scss)
          + builtins.readFile (builtins.fetchurl "https://github.com/black7375/Firefox-UI-Fix/raw/master/css/leptonChrome.css");
        userContent = builtins.readFile (builtins.fetchurl "https://github.com/black7375/Firefox-UI-Fix/raw/master/css/leptonContent.css");
      };
    };
  }; # }}}

  programs.librewolf.enable = config.hostname == "machine-nixos";

  xdg.configFile."discord/settings.json".text = builtins.toJSON {
    SKIP_HOST_UPDATE = true;
    enableHardwareAcceleration = false;
    openasar = {
      setup = true;
      cmdPreset = "battery";
    };
  };
}
