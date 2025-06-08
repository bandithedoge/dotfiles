{
  pkgs,
  config,
  inputs,
  ...
}:
{
  home.sessionVariables.BROWSER = "librewolf";

  programs = {
    librewolf = {
      # {{{
      enable = true;
      languagePacks = [
        "en-US"
        "pl"
      ];
      profiles = {
        default = {
          name = "default";
          id = 0;
          extensions.packages = with pkgs.bandithedoge.firefoxAddons; [
            base64-decoder
            betterviewer
            enhanced-github
            gitako
            imagus
            indie-wiki-buddy
            lovely-forks
            nexusmods-advance
            privacy-pass
            pronoundb
            reddit-enhancement-suite
            refined-github
            search_by_image
            skip-redirect
            sponsorblock
            stylus
            ublock-origin
            violentmonkey
          ];
          settings = {
            "browser.autofocus" = false;
            "devtools.chrome.enabled" = true;
            "devtools.debugger.remote-enabled" = true;
            "dom.security.https_only_mode" = true;
            "extensions.webextensions.ExtensionStorageIDB.enabled" = false;
            "font.name.monospace.x-western" = pkgs.rice.monoFont;
            "font.name.sans-serif.x-western" = pkgs.rice.uiFont;
            "font.name.serif.x-western" = pkgs.rice.serifFont;
            "general.autoScroll" = true;
            "layers.acceleration.force-enabled" = true;
            "sidebar.revamp" = true;
            "sidebar.verticalTabs" = true;
            "sidebar.visibility" = "expand-on-hover";
            "toolkit.tabbox.switchByScrolling" = true;
            "ui.context_menus.after_mouseup" = true;
            "ui.key.menuAccessKeyFocuses" = false;

            "identity.fxaccounts.enabled" = true;
            "privacy.clearOnShutdown.cookies" = false;
            "privacy.clearOnShutdown.downloads" = false;
            "privacy.clearOnShutdown.history" = false;
            "privacy.fingerprintingProtection" = true;
            "privacy.fingerprintingProtection.overrides" = "+AllTargets,-CSSPrefersColorScheme";
            "privacy.resistFingerprinting" = false;
            "webgl.disabled" = false;

            # smoothfox
            "apz.overscroll.enabled" = true;
            "general.smoothScroll" = true;
            "general.smoothScroll.currentVelocityWeighting" = "0.15";
            "general.smoothScroll.mouseWheel.durationMinMS" = 80;
            "general.smoothScroll.stopDecelerationWeighting" = "0.6";
            "mousewheel.min_line_scroll_amount" = 10;

            # https://github.com/black7375/Firefox-UI-Fix/blob/master/user.js
            "browser.compactmode.show" = true;
            "browser.newtabpage.activity-stream.improvesearch.handoffToAwesomebar" = false;
            "layout.css.has-selector.enabled" = true;
            "svg.context-properties.content.enabled" = true;
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

            # https://github.com/black7375/Firefox-UI-Fix/wiki/Options
            "userChrome.autohide.bookmarkbar" = true;
            "userChrome.autohide.page_action" = true;
            "userChrome.centered.urlbar" = true;
            "userChrome.decoration.animate" = true;
            "userChrome.decoration.cursor" = true;
            "userChrome.decoration.download_panel" = true;
            "userChrome.decoration.field_border" = true;
            "userChrome.panel.remove_strip" = true;
            "userChrome.theme.built_in_contrast" = true;
            "userChrome.theme.fully_color" = true;
            "userChrome.theme.fully_dark" = true;
            "userChrome.theme.proton_chrome" = true;
            "userChrome.theme.proton_color" = true;
            "userChrome.urlView.go_button_when_typing" = true;
            "userChrome.urlView.move_icon_to_left" = true;
            "userContent.newTab.field_border" = true;
            "userContent.page.dark_mode" = true;
            "userContent.page.illustration" = true;
            "userContent.page.proton" = true;
            "userContent.page.proton_color" = true;
            "userContent.player.animate" = true;
            "userContent.player.click_to_play" = true;
            "userContent.player.icon" = true;
            "userContent.player.noaudio" = true;
            "userContent.player.size" = true;
            "userContent.player.ui" = true;
          };
          preConfig = builtins.readFile (inputs.betterfox + "/user.js");
          # + builtins.readFile (inputs.firefox-ui-fix + "/user.js");
          userChrome =
            builtins.readFile (inputs.firefox-ui-fix + "/css/leptonChrome.css")
            + builtins.readFile (pkgs.rice.compileSCSS "firefox.css" ./firefox.scss);
          userContent = builtins.readFile (inputs.firefox-ui-fix + "/css/leptonContent.css");
        };
        secondary.id = 1;
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
      "equibop/settings.json".text =
        let
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

      "equibop/settings/quickCss.css".source = pkgs.rice.compileSCSS "discord.css" ./discord.scss;

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
