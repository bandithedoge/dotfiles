{
  pkgs,
  config,
  ...
}: {
  programs = {
    kitty = {
      # {{{
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
        # shell = "zellij";
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

    firefox = {
      # {{{
      enable = config.hostname == "machine-nixos";
      package =
        if !pkgs.stdenv.isDarwin
        then pkgs.firefox
        else pkgs.dummy;
      nativeMessagingHosts = with pkgs; [tridactyl-native];
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
            indie-wiki-buddy
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
            "font.name.monospace.x-western" = pkgs.rice.monoFont;
            "font.name.sans-serif.x-western" = pkgs.rice.uiFont;
            "font.name.serif.x-western" = pkgs.rice.serifFont;
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
            "userChrome.autohide.bookmarkbar" = false;
            "userChrome.autohide.forward_button" = true;
            "userChrome.autohide.page_action" = true;
            "userChrome.autohide.sidebar" = true;
            "userChrome.autohide.toolbar_overlap" = true;
            "userChrome.centered.urlbar" = true;
            "userChrome.hidden.sidebar_header" = true;
            "userChrome.hidden.tabbar" = true;
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

    librewolf.enable = config.hostname == "machine-nixos";

    wezterm = {
      enable = true;
      extraConfig =
        pkgs.rice.def.lua
        + builtins.readFile
        (pkgs.runCommand "wezterm.fnl" {
            nativeBuildInputs = with pkgs; [fennel];
          } ''
            fennel -c ${./wezterm.fnl} > $out
          '');
    };
  };
  xdg = {
    configFile = {
      "discord/settings.json".text = builtins.toJSON {
        SKIP_HOST_UPDATE = true;
        enableHardwareAcceleration = false;
        openasar = {
          setup = true;
          cmdPreset = "battery";
        };
      };

      "vesktop/settings.json".text = let
        color = c: "rgb(${pkgs.colors.conversions.hexToRGBString ", " (pkgs.lib.removePrefix "#" c)})";
      in
        with pkgs.rice;
          builtins.toJSON {
            minimizeToTray = "on";
            discordBranch = "stable";
            arRPC = "on";
            splashColor = color base05;
            splashBackground = color base00;
            splashTheming = true;
            enableMenu = true;
            hardwareAcceleration = false;
          };

      "vesktop/settings/quickCss.css".source = pkgs.rice.compileSCSS ./discord.scss;
    };
  };
}
