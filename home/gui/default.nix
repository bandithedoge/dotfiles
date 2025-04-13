{
  pkgs,
  config,
  flake,
  ...
}: {
  programs = {
    floorp = {
      # {{{
      enable = config.hostname == "machine-nixos";
      package =
        if !pkgs.stdenv.isDarwin
        then pkgs.floorp
        else pkgs.dummy;
      nativeMessagingHosts = with pkgs; [tridactyl-native];
      languagePacks = ["en-US" "pl"];
      policies = {
        AppAutoUpdate = false;
        DisableFirefoxStudies = true;
        DisableTelemetry = true;
        DisableFeedbackCommands = true;
        DisablePocket = true;
        DisableSetDesktopBackground = true;
        DontCheckDefaultBrowser = true;
        ManualAppUpdateOnly = true;
      };
      profiles = {
        default = {
          name = "default";
          isDefault = true;
          extensions.packages = with pkgs.bandithedoge.firefoxAddons; [
            augmented-steam
            base64-decoder
            betterviewer
            csgofloat
            enhanced-github
            gesturefy
            gitako
            imagus
            indie-wiki-buddy
            lovely-forks
            material-icons-for-github
            nexusmods-advance
            privacy-pass
            pronoundb
            reddit-enhancement-suite
            refined-github
            search_by_image
            skip-redirect
            sponsorblock
            steam-database
            tridactyl
            ublock-origin
            violentmonkey
          ];
          settings = {
            "browser.autofocus" = false;
            "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" = false;
            "devtools.chrome.enabled" = true;
            "extensions.webextensions.ExtensionStorageIDB.enabled" = false;
            "font.name.monospace.x-western" = pkgs.rice.monoFont;
            "font.name.sans-serif.x-western" = pkgs.rice.uiFont;
            "font.name.serif.x-western" = pkgs.rice.serifFont;
            "general.autoScroll" = true;
            "layers.acceleration.force-enabled" = true;
            "layout.css.has-selector.enabled" = true;
            "svg.context-properties.content.enabled" = true;
            "toolkit.tabbox.switchByScrolling" = true;
            "ui.context_menus.after_mouseup" = true;
            "ui.key.menuAccessKeyFocuses" = false;
            "widget.non-native-theme.use-theme-accent" = true;

            # fastfox
            "browser.cache.disk.free_space_hard_limit" = 2048;
            "browser.cache.disk.free_space_soft_limit" = 10240;
            "browser.cache.jsbc_compression_level" = 3;
            "browser.cache.memory.max_entry_size" = 10240;
            "browser.low_commit_space_threshold_mb" = 3726;
            "browser.low_commit_space_threshold_percent" = 20;
            "browser.tabs.min_inactive_duration_before_unload" = 300000;
            "gfx.webrender.all" = true;
            "gfx.webrender.compositor" = true;
            "gfx.webrender.compositor.force-enabled" = true;
            "gfx.webrender.precache-shaders" = true;
            "layers.gpu-process.enabled" = true;
            "layers.gpu-process.force-enabled" = true;
            "layers.mlgpu.enabled" = true;
            "media.ffmpeg.vaapi.enabled" = true;
            "media.gpu-process-decoder" = true;
            "media.hardware-video-decoding.enabled" = true;
            "media.hardware-video-decoding.force-enabled" = true;
            "media.memory_cache_max_size" = 65536;
            "network.dnsCacheEntries" = 1000;
            "network.dnsCacheExpirationGracePeriod" = 240;
            "nglayout.initialpaint.delay" = 5;

            # peskyfox
            "accessibility.typeaheadfind" = false;
            "browser.download.alwaysOpenPanel" = false;
            "browser.download.always_ask_before_handling_new_types" = true;
            "browser.download.folderList" = 2;
            "browser.download.manager.addToRecentDocs" = true;
            "browser.download.useDownloadDir" = false;
            "browser.messaging-system.whatsNewPanel.enabled" = false;
            "browser.newtabpage.activity-stream.asrouter.devtoolsEnabled" = true;
            "browser.newtabpage.enabled" = false;
            "browser.startup.homepage" = "about:blank";
            "browser.startup.page" = 3;
            "browser.tabs.tabMinWidth" = 120;
            "browser.urlbar.addons.featureGate" = false;
            "browser.urlbar.clipboard.featureGate" = false;
            "browser.urlbar.mdn.featureGate" = false;
            "browser.urlbar.suggest.engines" = false;
            "browser.urlbar.suggest.trending" = false;
            "browser.urlbar.suggest.weather" = false;
            "browser.vpn_promo.enabled" = false;
            "devtools.debugger.ui.editor-wrapping" = true;
            "devtools.inspector.showAllAnonymousContent" = true;
            "devtools.inspector.showUserAgentStyles" = true;
            "gfx.webrender.quality.force-subpixel-aa-where-possible" = true;
            "image.jxl.enabled" = true;
            "layout.css.moz-document.content.enabled" = true;
            "layout.css.prefers-color-scheme.content-override" = 0;
            "middlemouse.contentLoadURL" = false;
            "pdfjs.sidebarViewOnLoad" = 2;
            "reader.parse-on-load.enabled" = false;
            "startup.homepage_welcome_url" = "";
            "startup.homepage_welcome_url.additional" = "";
            "toolkit.zoomManager.zoomValues" = ".3,.5,.67,.8,.9,.95,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,2,2.4,3";
            "view_source.wrap_long_lines" = true;

            # securefox
            "accessibility.force_disabled" = 1;
            "app.update.auto" = false;
            "browser.contentblocking.report.lockwise.enabled" = false;
            "browser.contentblocking.report.lockwise.how_it_works.url" = "";
            "browser.search.serpEventTelemetryCategorization.enabled" = false;
            "browser.search.suggest.enabled" = true;
            "browser.search.suggest.enabled.private" = true;
            "browser.tabs.searchclipboardfor.middleclick" = false;
            "captivedetect.canonicalURL" = "";
            "devtools.accessibility.enabled" = false;
            "dom.security.https_only_mode" = true;
            "dom.security.https_only_mode_error_page_user_suggestions" = true;
            "extensions.enabledScopes" = 7;
            "extensions.formautofill.addresses.enabled" = false;
            "extensions.formautofill.creditCards.enabled" = false;
            "extensions.update.enabled" = false;
            "extensions.webextensions.restrictedDomains" = "";
            "geo.provider.network.logging.enabled" = true;
            "network.captive-portal-service.enabled" = false;
            "network.cookie.sameSite.laxByDefault" = true;
            "network.cookie.sameSite.schemeful" = true;
            "network.proxy.socks_remote_dns" = true;
            "network.trr.builtin-excluded-domains" = "localhost,local";
            "network.trr.max-fails" = 5;
            "network.trr.mode" = 0;
            "network.trr.resolvers" = "[{ \"name\": \"Cloudflare\", \"url\": \"https://mozilla.cloudflare-dns.com/dns-query\" },{ \"name\": \"SecureDNS\", \"url\": \"https://doh.securedns.eu/dns-query\" },{ \"name\": \"AppliedPrivacy\", \"url\": \"https://doh.appliedprivacy.net/query\" },{ \"name\": \"Digitale Gesellschaft (CH)\", \"url\": \"https://dns.digitale-gesellschaft.ch/dns-query\" }, { \"name\": \"Quad9\", \"url\": \"https://dns.quad9.net/dns-query\" }]\n";
            "network.trr.uri" = "https://dns.dnswarden.com/00000000000000000000048";
            "permissions.default.desktop-notification" = 0;
            "permissions.default.geo" = 0;
            "permissions.manager.defaultsUrl" = "";
            "privacy.query_stripping.strip_on_share.enabled" = true;
            "privacy.trackingprotection.lower_network_priority" = true;
            "privacy.webrtc.globalMuteToggles" = true;
            "security.cert_pinning.enforcement_level" = 2;
            "security.mixed_content.block_display_content" = false;
            "security.sandbox.gpu.level" = 1;
            "signon.autofillForms" = false;
            "signon.autofillForms.http" = false;
            "signon.autologin.proxy" = false;
            "signon.firefoxRelay.feature" = "";
            "signon.generation.enabled" = false;
            "signon.management.page.breach-alerts.enabled" = false;
            "signon.management.page.breachAlertUrl" = "";
            "signon.rememberSignons" = false;
            "signon.schemeUpgrades" = false;
            "signon.showAutoCompleteFooter" = false;
            "signon.storeWhenAutocompleteOff" = false;
            "webchannel.allowObject.urlWhitelist" = "";

            # smoothfox
            "apz.overscroll.enabled" = true;
            "general.smoothScroll" = true;
            "general.smoothScroll.currentVelocityWeighting" = "0.15";
            "general.smoothScroll.mouseWheel.durationMinMS" = 80;
            "general.smoothScroll.stopDecelerationWeighting" = "0.6";
            "mousewheel.min_line_scroll_amount" = 10;

            "floorp.browser.sidebar.enable" = false;
            "floorp.browser.tabbar.settings" = 2;
            "floorp.browser.tabs.verticaltab" = true;
            "floorp.browser.workspaces.enabled" = false;
            "floorp.download.notification" = 3;
            "floorp.lepton.interface" = 3;
            "floorp.tabbar.style" = 2;
            "floorp.tabscroll.wrap" = true;
            "floorp.tabsleep.enabled" = true;
            "floorp.verticaltab.hover.enabled" = true;

            "userChrome.autohide.back_button" = true;
            "userChrome.autohide.forward_button" = true;
            "userChrome.autohide.page_action" = true;
            "userChrome.autohide.sidebar" = false;
            "userChrome.autohide.tab" = false;
            "userChrome.centered.bookmarkbar" = false;
            "userChrome.centered.tab" = false;
            "userChrome.centered.urlbar" = true;
            "userChrome.hidden.bookmarkbar_icon" = false;
            "userChrome.hidden.bookmarkbar_label" = false;
            "userChrome.hidden.disabled_menu" = false;
            "userChrome.hidden.navbar" = false;
            "userChrome.hidden.sidebar_header" = false;
            "userChrome.hidden.tab_icon" = false;
            "userChrome.hidden.tabbar" = false;
            "userChrome.hidden.urlbar_iconbox" = false;
            "userChrome.icon.disabled" = false;
            "userChrome.tab.bottom_rounded_corner" = false;
            "userChrome.tab.box_shadow" = false;
            "userChrome.tab.connect_to_window" = false;
            "userChrome.tab.lepton_like_padding" = false;
            "userChrome.tab.newtab_button_like_tab" = false;
            "userChrome.tab.newtab_button_proton" = true;
            "userChrome.tabbar.one_liner" = false;
            "userChrome.urlView.always_show_page_actions" = false;
            "userChrome.urlView.move_icon_to_left" = false;
          };
          preConfig =
            builtins.readFile (flake.inputs.betterfox + "/Fastfox.js")
            + builtins.readFile (flake.inputs.betterfox + "/Peskyfox.js")
            + builtins.readFile (flake.inputs.betterfox + "/Securefox.js")
            + ''
              user_pref("identity.fxaccounts.enabled", true);
            '';
          userChrome =
            builtins.readFile (pkgs.rice.compileSCSS ./firefox.scss);
        };
      };
    }; # }}}

    ghostty = {
      # {{{
      enable = true;
      enableFishIntegration = true;
      settings = with pkgs.rice; {
        font-family = monoFont;
        font-size = 12;
        adjust-cell-width = -1;
        mouse-hide-while-typing = true;
        window-padding-x = 5;
        window-padding-y = 5;
        window-padding-balance = true;
        window-decoration = false;
        # window-theme = "ghostty";
        resize-overlay = "never";
        bold-is-bright = false;
        auto-update = "off";

        background = base00;
        foreground = base05;
        cursor-color = base0F;
        selection-background = base05;
        selection-foreground = base00;
        palette = [
          "0=${base01}"
          "1=${base08}"
          "2=${base0B}"
          "3=${base09}"
          "4=${base0D}"
          "5=${base0E}"
          "6=${base0C}"
          "7=${base06}"

          "8=${base02}"
          "9=${base12}"
          "10=${base14}"
          "11=${base13}"
          "12=${base16}"
          "13=${base17}"
          "14=${base15}"
          "15=${base0F}"
        ];

        linux-cgroup = "never";
        gtk-single-instance = true;
        gtk-tabs-location = "bottom";
        adw-toolbar-style = "flat";
        class = "ghostty";
      };
    }; # }}}
  };
  xdg = {
    configFile = {
      "equibop/settings.json".text = let
        color = c: "rgb(${pkgs.colors.conversions.hexToRGBString ", " (pkgs.lib.removePrefix "#" c)})";
      in
        with pkgs.rice;
          builtins.toJSON {
            minimizeToTray = true;
            discordBranch = "stable";
            arRPC = "on";
            splashColor = color base05;
            splashBackground = color base00;
            splashTheming = true;
            enableMenu = true;
            hardwareAcceleration = false;
            disableMinSize = true;
            trayColor = pkgs.lib.removePrefix "#" base0F;
          };

      "equibop/settings/quickCss.css".source = pkgs.rice.compileSCSS ./discord.scss;

      "discord/settings.json".text = builtins.toJSON {
        SKIP_HOST_UPDATE = true;
        DANGEROUS_ENABLE_DEVTOOLS_ONLY_ENABLE_IF_YOU_KNOW_WHAT_YOURE_DOING = true;
        OPEN_ON_STARTUP = false;
        BACKGROUND_COLOR = pkgs.rice.base00;
        enableHardwareAcceleration = false;
        openasar = {
          setup = true;
          cmdPreset = "battery";
        };
      };
    };
  };
}
