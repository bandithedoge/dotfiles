{
  config,
  pkgs,
  ...
}: let
  rice = import ../../../../rice.nix;
in {
  home = {
    packages = with pkgs; [
      cutter
      discord
      ghidra
      icon-library
      imv
      libappindicator
      libappindicator-gtk3
      pavucontrol
      tigervnc
      wine
    ];
    sessionVariables = {
      XDG_CURRENT_DESKTOP = "Unity";
    };
  };

  # gtk {{{
  gtk = {
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
      package = pkgs.materia-theme;
      name = "Materia-dark";
    };
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
      gtk-enable-primary-paste = false;
      gtk-can-change-accels = 1;
      gtk-toolbar-style = "";
      gtk-decoration-layout = "menu";
    };
  }; # }}}

  programs.qutebrowser = {
    # {{{
    enable = true;
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
    package = pkgs.firefox-unwrapped;
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
    };
  };
  # }}}

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
}
