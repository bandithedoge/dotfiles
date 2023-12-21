{
  pkgs,
  config,
  ...
}: {
  programs.kitty = {
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
        blinking = "On";
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
          canvasblocker
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
          "browser.autofocus" = false;
          "browser.compactmode.show" = true;
          "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" = false;
          "devtools.chrome.enabled" = true;
          "general.autoScroll" = true;
          "gfx.webrender.all" = true;
          "layers.acceleration.force-enabled" = true;
          "layout.css.has-selector.enabled" = true;
          "svg.context-properties.content.enabled" = true;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "ui.context_menus.after_mouseup" = true;
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
          # ffprofile.com output
          ''
            user_pref("app.normandy.api_url", "");
            user_pref("app.normandy.enabled", false);
            user_pref("app.shield.optoutstudies.enabled", false);
            user_pref("app.update.auto", false);
            user_pref("beacon.enabled", false);
            user_pref("breakpad.reportURL", "");
            user_pref("browser.aboutConfig.showWarning", false);
            user_pref("browser.cache.offline.enable", false);
            user_pref("browser.crashReports.unsubmittedCheck.autoSubmit", false);
            user_pref("browser.crashReports.unsubmittedCheck.autoSubmit2", false);
            user_pref("browser.crashReports.unsubmittedCheck.enabled", false);
            user_pref("browser.disableResetPrompt", true);
            user_pref("browser.newtab.preload", false);
            user_pref("browser.newtabpage.activity-stream.section.highlights.includePocket", false);
            user_pref("browser.newtabpage.enabled", false);
            user_pref("browser.newtabpage.enhanced", false);
            user_pref("browser.newtabpage.introShown", true);
            user_pref("browser.safebrowsing.appRepURL", "");
            user_pref("browser.safebrowsing.blockedURIs.enabled", false);
            user_pref("browser.safebrowsing.downloads.enabled", false);
            user_pref("browser.safebrowsing.downloads.remote.enabled", false);
            user_pref("browser.safebrowsing.downloads.remote.url", "");
            user_pref("browser.safebrowsing.enabled", false);
            user_pref("browser.safebrowsing.malware.enabled", false);
            user_pref("browser.safebrowsing.phishing.enabled", false);
            user_pref("browser.selfsupport.url", "");
            user_pref("browser.send_pings", false);
            user_pref("browser.sessionstore.privacy_level", 0);
            user_pref("browser.shell.checkDefaultBrowser", false);
            user_pref("browser.startup.homepage_override.mstone", "ignore");
            user_pref("browser.tabs.crashReporting.sendReport", false);
            user_pref("browser.urlbar.groupLabels.enabled", false);
            user_pref("browser.urlbar.quicksuggest.enabled", false);
            user_pref("browser.urlbar.speculativeConnect.enabled", false);
            user_pref("browser.urlbar.trimURLs", false);
            user_pref("datareporting.healthreport.service.enabled", false);
            user_pref("datareporting.healthreport.uploadEnabled", false);
            user_pref("datareporting.policy.dataSubmissionEnabled", false);
            user_pref("device.sensors.ambientLight.enabled", false);
            user_pref("device.sensors.enabled", false);
            user_pref("device.sensors.motion.enabled", false);
            user_pref("device.sensors.orientation.enabled", false);
            user_pref("device.sensors.proximity.enabled", false);
            user_pref("dom.battery.enabled", false);
            user_pref("dom.event.clipboardevents.enabled", false);
            user_pref("experiments.activeExperiment", false);
            user_pref("experiments.enabled", false);
            user_pref("experiments.manifest.uri", "");
            user_pref("experiments.supported", false);
            user_pref("extensions.blocklist.enabled", false);
            user_pref("extensions.getAddons.cache.enabled", false);
            user_pref("extensions.getAddons.showPane", false);
            user_pref("extensions.greasemonkey.stats.optedin", false);
            user_pref("extensions.greasemonkey.stats.url", "");
            user_pref("extensions.pocket.enabled", false);
            user_pref("extensions.shield-recipe-client.api_url", "");
            user_pref("extensions.shield-recipe-client.enabled", false);
            user_pref("extensions.webservice.discoverURL", "");
            user_pref("media.autoplay.default", 0);
            user_pref("media.autoplay.enabled", true);
            user_pref("media.navigator.enabled", false);
            user_pref("media.peerconnection.enabled", false);
            user_pref("network.allow-experiments", false);
            user_pref("network.captive-portal-service.enabled", false);
            user_pref("network.cookie.cookieBehavior", 1);
            user_pref("network.dns.disablePrefetch", true);
            user_pref("network.dns.disablePrefetchFromHTTPS", true);
            user_pref("network.http.referer.spoofSource", true);
            user_pref("network.http.speculative-parallel-limit", 0);
            user_pref("network.predictor.enable-prefetch", false);
            user_pref("network.predictor.enabled", false);
            user_pref("network.prefetch-next", false);
            user_pref("network.trr.mode", 5);
            user_pref("privacy.donottrackheader.enabled", true);
            user_pref("privacy.donottrackheader.value", 1);
            user_pref("privacy.query_stripping", true);
            user_pref("privacy.resistFingerprinting", true);
            user_pref("privacy.trackingprotection.cryptomining.enabled", true);
            user_pref("privacy.trackingprotection.enabled", true);
            user_pref("privacy.trackingprotection.fingerprinting.enabled", true);
            user_pref("privacy.trackingprotection.pbmode.enabled", true);
            user_pref("privacy.usercontext.about_newtab_segregation.enabled", true);
            user_pref("security.ssl.disable_session_identifiers", true);
            user_pref("services.sync.prefs.sync.browser.newtabpage.activity-stream.showSponsoredTopSite", false);
            user_pref("signon.autofillForms", false);
            user_pref("toolkit.telemetry.archive.enabled", false);
            user_pref("toolkit.telemetry.bhrPing.enabled", false);
            user_pref("toolkit.telemetry.cachedClientID", "");
            user_pref("toolkit.telemetry.enabled", false);
            user_pref("toolkit.telemetry.firstShutdownPing.enabled", false);
            user_pref("toolkit.telemetry.hybridContent.enabled", false);
            user_pref("toolkit.telemetry.newProfilePing.enabled", false);
            user_pref("toolkit.telemetry.prompted", 2);
            user_pref("toolkit.telemetry.rejected", true);
            user_pref("toolkit.telemetry.reportingpolicy.firstRun", false);
            user_pref("toolkit.telemetry.server", "");
            user_pref("toolkit.telemetry.shutdownPingSender.enabled", false);
            user_pref("toolkit.telemetry.unified", false);
            user_pref("toolkit.telemetry.unifiedIsOptIn", false);
            user_pref("toolkit.telemetry.updatePing.enabled", false);
            user_pref("webgl.renderer-string-override", " ");
            user_pref("webgl.vendor-string-override", " ");
          '';
        userChrome =
          builtins.readFile (pkgs.rice.compileSCSS ./firefox.scss)
          + builtins.readFile (builtins.fetchurl "https://github.com/black7375/Firefox-UI-Fix/raw/master/css/leptonChrome.css");
        userContent = builtins.readFile (builtins.fetchurl "https://github.com/black7375/Firefox-UI-Fix/raw/master/css/leptonContent.css");
      };
    };
  }; # }}}

  programs.librewolf.enable = true;
}
