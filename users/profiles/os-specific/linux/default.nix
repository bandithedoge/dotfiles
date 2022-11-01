{
  config,
  pkgs,
  ...
}: let
  rice = import ../../../../rice.nix {inherit pkgs;};
in {
  imports = [./audio.nix ./gaming.nix ./x/default.nix];

  home = {
    packages = with pkgs; [
      blender
      bottles
      dfeet
      discord-canary
      ferdium
      flowblade
      ghidra
      gparted
      icon-library
      imv
      keepassxc
      krita
      nim
      oomoxFull
      pavucontrol
      pcmanfm
      rclone
      teams
      tigervnc
      transmission-gtk
    ];
    pointerCursor = {
      inherit (rice.gtk.cursorTheme) package name size;
      x11.enable = true;
      gtk.enable = true;
    };
  };

  gtk = {
    # {{{
    enable = true;
    font = {
      name = rice.uiFont;
      size = 12;
    };
    inherit (rice.gtk) theme iconTheme;
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
      gtk-enable-primary-paste = false;
      gtk-can-change-accels = 1;
      gtk-toolbar-style = "";
      gtk-decoration-layout = "menu";
    };
  }; # }}}

  qt = {
    # {{{
    enable = true;
    platformTheme = "gnome";
    style = {
      name = "adwaita-dark";
      package = pkgs.adwaita-qt;
    };
  };
  # }}}

  programs.qutebrowser = {
    # {{{
    enable = true;
    searchEngines = {
      DEFAULT = "https://serx.ml/search?q={}";
      g = "https://www.google.com/search?q={}";
      a = "https://wiki.archlinux.org/?search={}";
      n = "https://nixos.wiki/index.php?search={}";
    };
    settings = {
      changelog_after_upgrade = "patch";
      completion.timestamp_format = "%A %d %B %T";
      confirm_quit = ["downloads"];
      content = {
        blocking.method = "both";
        pdfjs = true;
      };
      downloads.position = "bottom";
      fonts = {
        default_family = rice.uiFont;
        default_size = "14px";
        hints = rice.monoFont;
      };
      hints.leave_on_load = true;
      new_instance_open_target = "tab-bg";
      qt.highdpi = true;
      scrolling.smooth = true;
      session.lazy_restore = true;
      statusbar.widgets = [
        "progress"
        "keypress"
        "url"
      ];
      tabs = {
        last_close = "startpage";
        position = "left";
        title.format = "{audio}{private}{current_title}";
      };
      url = {
        default_page = "https://serx.ml";
        open_base_url = true;
        start_pages = "https://serx.ml";
      };
      window = {
        hide_decoration = true;
        title_format = "{audio}{private}{current_title}";
        transparent = true;
      };
      colors = let
        blank = "#00000000";
      in
        with rice; {
          # {{{
          completion = {
            fg = base05;
            even.bg = base01;
            odd.bg = base01;
            category = {
              bg = base02;
              fg = base05;
              border = {
                bottom = blank;
                top = blank;
              };
            };
            item.selected = {
              bg = base0F;
              fg = base00;
              match.fg = base00;
              border = {
                bottom = blank;
                top = blank;
              };
            };
            match.fg = base0F;
            scrollbar = {
              bg = base01;
              fg = base0F;
            };
          };
          contextmenu = {
            disabled = {
              bg = blank;
              fg = base03;
            };
            menu = {
              bg = base02;
              fg = base05;
            };
            selected = {
              bg = base0F;
              fg = base00;
            };
          };
          downloads = {
            bar.bg = base10;
            error = {
              bg = base08;
              fg = base00;
            };
            start = {
              bg = base00;
              fg = base08;
            };
            system = {
              bg = "none";
              fg = "none";
            };
          };
          hints = {
            bg = base02;
            fg = base05;
            match.fg = base0F;
          };
          keyhint = {
            bg = base02;
            fg = base05;
            suffix.fg = base0F;
          };
          messages = {
            error = {
              bg = base08;
              fg = base00;
              border = base08;
            };
            info = {
              bg = base0D;
              fg = base00;
              border = base0D;
            };
            warning = {
              bg = base09;
              fg = base00;
              border = base09;
            };
          };
          prompts = {
            bg = base02;
            fg = base05;
            border = blank;
            selected = {
              bg = base0F;
              fg = base00;
            };
          };
          statusbar = {
            caret = {
              bg = base0A;
              fg = base00;
              selection = {
                bg = base09;
                fg = base00;
              };
            };
            command = {
              bg = base00;
              fg = base05;
              private = {
                bg = base00;
                fg = base05;
              };
            };
            insert = {
              bg = base0B;
              fg = base00;
            };
            normal = {
              bg = base00;
              fg = base04;
            };
            passthrough = {
              bg = base0B;
              fg = base00;
            };
            private = {
              bg = base0D;
              fg = base00;
            };
            progress.bg = base0F;
            url = {
              fg = base05;
              hover.fg = base0F;
              success.http.fg = base0F;
              success.https.fg = base0F;
              warn.fg = base08;
            };
          };
          tabs = {
            bar.bg = base10;
            even = {
              bg = base00;
              fg = base05;
            };
            odd = {
              bg = base00;
              fg = base05;
            };
            indicator = {
              error = base08;
              start = base01;
              system = "none";
            };
            pinned = {
              even = {
                bg = base02;
                fg = base05;
              };
              odd = {
                bg = base02;
                fg = base05;
              };
              selected = {
                even = {
                  bg = base0F;
                  fg = base00;
                };
                odd = {
                  bg = base0F;
                  fg = base00;
                };
              };
            };
            selected = {
              even = {
                bg = base0F;
                fg = base00;
              };
              odd = {
                bg = base0F;
                fg = base00;
              };
            };
          };
          webpage = {
            bg = base00;
            preferred_color_scheme = "dark";
          };
        };
      # }}}
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

  programs.firefox = {
    # {{{
    enable = true;
    package = pkgs.firefox-beta-bin.override {
      cfg.enableTridactylNative = true;
    };
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
    profiles = {
      default = {
        settings = {
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "layers.acceleration.force-enabled" = true;
          "gfx.webrender.all" = true;
          "svg.context-properties.content.enabled" = true;
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

  services.syncthing.enable = true;
}
