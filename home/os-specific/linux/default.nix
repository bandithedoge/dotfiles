{
  pkgs,
  config,
  flake,
  lib,
  ...
}: {
  imports = [
    ./audio.nix
    ./wayland
  ];

  home = {
    packages = with pkgs; [
      appimage-run
      bandithedoge.chainner-bin
      bandithedoge.deemix-gui-bin
      bandithedoge.propertree
      bleachbit
      # blender-hip
      caprine
      czkawka
      d-spy
      fractal
      gimp
      gnome-firmware
      gnome-graphs
      handbrake
      icon-library
      inkscape
      keepassxc
      keepmenu
      krita
      kvirc
      libnotify
      libreoffice-fresh
      matlab
      nicotine-plus
      nix-alien
      pciutils
      prusa-slicer
      qbittorrent
      qview
      telegram-desktop_git
      vesktop
      wine-ge
      winetricks
      wtype
      xdragon
      zenity
    ];

    pointerCursor = {
      inherit (pkgs.rice.cursorTheme) package name size;
      gtk.enable = true;
    };
  };

  services.flatpak = {
    # {{{
    enable = true;
    uninstallUnmanaged = true;
    remotes = [
      {
        name = "flathub";
        location = "https://dl.flathub.org/repo/flathub.flatpakrepo";
      }
      {
        name = "flathub-beta";
        location = "https://flathub.org/beta-repo/flathub-beta.flatpakrepo";
      }
      {
        name = "gnome-nightly";
        location = "https://nightly.gnome.org/gnome-nightly.flatpakrepo";
      }
    ];
    overrides = {
      global = {
        Environment = {
          XCURSOR_PATH = "/run/host/user-share/icons:/run/host/share/icons";
        };
        Context.filesystems = [
          "/etc/profiles/per-user/bandithedoge/bin:ro"
          "/nix/store:ro"
          "/run/current-system/sw/bin:ro"
          "xdg-config/MangoHud:ro"
          "xdg-config/gtk-3.0:ro"
          "xdg-config/gtk-4.0:ro"
        ];
      };
      "org.jdownloader.JDownloader".Context.filesystems = [
        "/mnt"
      ];
    };
    packages = [
      "com.github.tchx84.Flatseal"
      "de.bforartists.Bforartists"
      "org.gtk.Gtk3theme.adw-gtk3-dark"
      "org.jdownloader.JDownloader"
      "re.sonny.Workbench"
    ];
  };
  # }}}

  # HACK: https://github.com/nix-community/home-manager/issues/2659
  systemd.user.sessionVariables = config.home.sessionVariables;

  # gtk {{{
  gtk = rec {
    enable = true;
    font = {
      name = pkgs.rice.uiFont;
      size = 13;
    };
    inherit (pkgs.rice) cursorTheme;
    inherit (pkgs.rice.gtk) theme iconTheme;
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = true;
      gtk-can-change-accels = 1;
      gtk-decoration-layout = "";
      gtk-dialogs-use-header = false;
      gtk-enable-primary-paste = false;
      gtk-toolbar-style = "";
    };
    gtk4 = gtk3;
  };

  dconf.settings = {
    "org/gnome/desktop/interface" = with pkgs.rice; {
      color-scheme = "prefer-dark";
      monospace-font-name = "${monoFont} 12";
    };
    "org/freedesktop/appearance".color-scheme = 1;
  };
  # }}}

  qt = {
    enable = true;
    platformTheme.name = "gtk3";
    style.name = "adwaita-dark";
  };

  xdg.configFile = let
    css = pkgs.rice.compileSCSS ../../../gtk.scss;
  in {
    "keepmenu/config.ini".text = lib.generators.toINI {} {
      dmenu.dmenu_command = "rofi -dmenu";
      database = {
        inherit (pkgs.rice) terminal;
        database_1 = "~/keepass/pass.kdbx";
        editor = config.home.sessionVariables.EDITOR;
        gui_editor = "keepassxc";
        type_library = "wtype";
      };
    };

    "gtk-4.0/gtk.css".source = css;
    "gtk-3.0/gtk.css".source = css;
  };

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-gtk
      xdg-desktop-portal-hyprland
      xdg-desktop-portal-wlr
    ];
    config.hyprland.default = ["hyprland" "gtk"];
  };

  programs = {
    qutebrowser = {
      # {{{
      enable = config.hostname == "thonkpad";
      searchEngines = {
        DEFAULT = "https://www.startpage.com/sp/search?query={}";
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

    mpv = {
      # {{{
      enable = true;
      scripts = with pkgs.mpvScripts; [
        uosc
        mpris
        thumbfast
        visualizer
        sponsorblock
        quality-menu
      ];
      config = {
        ao = "pipewire";
        border = false;
        hwdec = "auto-safe";
        osd-bar = false;
        profile = "sw-fast";
        save-position-on-quit = true;
        video-sync = "display-resample";
        vo = "gpu-next";
      };
      scriptOpts.uosc = {
        timeline_style = "bar";
        top_bar = "always";
        scale_fullscreen = 1;
        time_precision = 1;
        color = let
          color = pkgs.lib.removePrefix "#";
        in
          with pkgs.rice;
            pkgs.lib.concatStringsSep "," [
              "background=${color base00}"
              "background_text=${color base05}"
              "error=${color base08}"
              "foreground=${color base0F}"
              "foreground_text=${color base00}"
              "success=${color base0B}"
            ];
      };
    };
    # }}}

    zathura = {
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

    rofi = {
      # {{{
      enable = true;
      package = pkgs.rofi-wayland;
      font = "${pkgs.rice.uiFont} 12";
      plugins = with pkgs; [
        (rofi-calc.override {rofi-unwrapped = pkgs.rofi-wayland-unwrapped;})
      ];
      extraConfig = {
        modi = let
          power = pkgs.writeShellScript "rofi-power" ''
            if [ -z "$@" ]; then
              echo -en "\0prompt\x1fpower\n"

              echo -en "Lock\0icon\x1fsystem-lock-screen\n"
              echo -en "Reboot\0icon\x1fsystem-reboot\n"
              echo -en "Shutdown\0icon\x1fsystem-shutdown\n"
            else
              case "$*" in
                "Lock") loginctl lock-session; exit ;;
                "Reboot") systemctl reboot; exit ;;
                "Shutdown") systemctl poweroff; exit ;;
              esac
            fi
          '';
        in "power:${power},drun,calc";

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

    obs-studio = {
      enable = true;
      plugins = with pkgs.obs-studio-plugins; [
        wlrobs
        obs-gstreamer
        looking-glass-obs
      ];
    };
  };

  services = {
    dunst = with pkgs.rice; {
      enable = false;
      inherit (gtk) iconTheme;
      settings = {
        global = {
          follow = "keyboard";
          offset = "5x5";
          progress_bar_frame_width = 2;
          frame_width = 2;
          frame_color = base0F;
          font = uiFont;
          format = "<i>%a</i>\\n<b>%s</b>\\n%b";
          background = base02;
          foreground = base05;
        };
        urgency_low = {
          frame_color = base03;
          foreground = base03;
        };
        urgency_critical = {
          frame_color = base08;
          foreground = base08;
        };
      };
    };

    syncthing.enable = true;
    systembus-notify.enable = true;
    gnome-keyring.enable = true;
  };

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

  fonts.fontconfig = {
    enable = true;
    defaultFonts = with pkgs.rice; {
      monospace = [monoFont];
      sansSerif = [uiFont];
      serif = [serifFont];
    };
  };
}
