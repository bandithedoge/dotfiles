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

  programs.firefox = {
    # {{{
    enable = false;
    package =
      if !pkgs.stdenv.isDarwin
      then
        pkgs.firefox-beta-bin.override
        {
          cfg.enableTridactylNative = true;
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
          enhancer-for-youtube
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
          ruffle
          sourcegraph
          sponsorblock
          steam-database
          stylus
          tabcenter-reborn
          tridactyl
          ublock-origin
          violentmonkey
        ];
        settings = {
          "app.update.auto" = false;
          "browser.autofocus" = false;
          "gfx.webrender.all" = true;
          "layers.acceleration.force-enabled" = true;
          "svg.context-properties.content.enabled" = true;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "ui.context_menus.after_mouseup" = true;
        };
        userChrome = ''
          #titlebar, #sidebar-header {
            display: none;
          }
        '';
      };
    };
  }; # }}}
}
