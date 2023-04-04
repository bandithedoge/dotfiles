{
  pkgs,
  config,
  ...
}: let
  rofi-stuff = pkgs.callPackage ./rofi {};
in {
  imports = [
    ./audio.nix
    # ./wayland.nix
    ./x
  ];

  home = {
    packages = with pkgs; [
      anydesk
      blender
      cutter
      dfeet
      discord-canary
      ferdium
      flowblade
      ghidra
      gparted
      jadx
      keepassxc
      krita
      libreoffice-fresh
      nim
      pavucontrol
      pciutils
      rclone
      tigervnc
      transmission-gtk
    ];
    pointerCursor = {
      inherit (pkgs.rice.gtk.cursorTheme) package name size;
      x11.enable = true;
      gtk.enable = true;
    };
  };

  # gtk {{{
  gtk = {
    enable = true;
    font = {
      name = pkgs.rice.uiFont;
      size = 12;
    };
    inherit (pkgs.rice.gtk) theme iconTheme;
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
      gtk-enable-primary-paste = false;
      gtk-can-change-accels = 1;
      gtk-toolbar-style = "";
      gtk-decoration-layout = "menu";
    };
  };

  xdg.configFile = let
    css = pkgs.rice.compileSCSS ./gtk.scss;
  in {
    "gtk-4.0/gtk.css".source = css;
    "gtk-3.0/gtk.css".source = css;
  };
  # }}}

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
        default_family = pkgs.rice.uiFont;
        default_size = "14px";
        hints = pkgs.rice.monoFont;
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
        with pkgs.rice; {
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
    options = with pkgs.rice; {
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
    mappings = {
      "-" = "zoom out";
      "=" = "zoom in";
    };
  };
  # }}}

  programs.rofi = {
    # {{{
    enable = true;
    package = pkgs.rofi-wayland;
    font = "${pkgs.rice.uiFont} 12";
    plugins = with pkgs; [rofi-calc];
    extraConfig = {
      modi = "power:${rofi-stuff}/bin/power,drun,run,calc";

      drun-match-fields = "name,exec";
      drun-display-format = "{name}";
      show-icons = true;
      scroll-method = 1;

      kb-mode-complete = "";
      kb-remove-to-eol = "";
      kb-remove-to-sol = "";
      kb-remove-char-forward = "Delete";
      kb-row-up = "Control+k";
      kb-row-down = "Control+j";
      kb-mode-next = "Control+l";
      kb-mode-previous = "Control+h";
      kb-page-prev = "Control+u";
      kb-page-next = "Control+d";
      kb-accept-entry = "Return";
      kb-remove-char-back = "BackSpace";
      kb-screenshot = "Control+Shift+s";
    };
    theme = with pkgs.rice; let
      inherit (config.lib.formats.rasi) mkLiteral;
      padding = mkLiteral "5px";
    in {
      "*" = {
        background-color = mkLiteral base10;
        text-color = mkLiteral base05;
      };

      window = {
        border = mkLiteral "2px";
        children = map mkLiteral ["mainbox"];
        border-color = mkLiteral base0F;
      };

      mainbox = {
        children = map mkLiteral ["inputbar" "message" "listview" "mode-switcher"];
        spacing = padding;
        margin = padding;
      };

      inputbar = {
        children = map mkLiteral ["prompt" "entry"];
        spacing = mkLiteral "0";
      };

      prompt = {
        background-color = mkLiteral base0F;
        text-color = mkLiteral base00;
        vertical-align = mkLiteral "0.5";
        horizontal-align = mkLiteral "0.5";
        inherit padding;
      };

      entry = {
        background-color = mkLiteral base00;
        vertical-align = mkLiteral "0.5";
        inherit padding;
      };

      listview = {
        background-color = mkLiteral base00;
        scrollbar = true;
        spacing = mkLiteral "0px 5px";
        inherit padding;
      };

      element = {
        children = map mkLiteral ["element-icon" "element-text"];
        background-color = mkLiteral "transparent";
        padding = mkLiteral "2px";
        spacing = padding;
      };
      element-icon = {
        background-color = mkLiteral "inherit";
        size = mkLiteral "1.25em";
      };
      element-text = {
        background-color = mkLiteral "inherit";
        highlight = mkLiteral "bold ${base0F}";
        vertical-align = mkLiteral "0.5";
      };

      "element.selected.normal".background-color = mkLiteral base02;
      "element.urgent".text-color = mkLiteral base08;
      "element.active".text-color = mkLiteral base0B;
      "element.selected.urgent".background-color = mkLiteral base08;
      "element.selected.active".background-color = mkLiteral base0B;

      scrollbar = {
        background-color = mkLiteral base00;
        handle-color = mkLiteral base0F;
        handle-width = mkLiteral "5px";
      };

      button = {
        background-color = mkLiteral base01;
        text-color = mkLiteral base03;
      };
      "button.selected" = {
        background-color = mkLiteral base0F;
        text-color = mkLiteral base00;
      };
    };
  }; # }}}

  # polkit {{{
  systemd.user.services.polkit-gnome-authentication-agent-1 = {
    Unit = {
      Description = "polkit-gnome-authentication-agent-1";
      After = ["graphical-session.target"];
    };

    Install = {
      WantedBy = ["graphical-session.target"];
      Wants = ["graphical-session.target"];
      After = ["graphical-session.target"];
    };

    Service = {
      Type = "simple";
      ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
      Restart = "on-failure";
      RestartSec = 1;
      TimeoutStopSec = 10;
    };
  };
  # }}}

  services.syncthing.enable = true;
}
