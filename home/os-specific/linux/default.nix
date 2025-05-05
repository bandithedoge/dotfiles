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
      blender-hip
      caprine
      d-spy
      equibop
      foliate
      fractal
      gimp3
      gnome-secrets
      handbrake
      icon-library
      inkscape
      keepmenu
      krita
      kvirc
      libnotify
      newsflash
      nicotine-plus
      nix-alien
      pciutils
      qbittorrent
      qt6ct
      rice.monoFontPackage
      rice.uiFontPackage
      telegram-desktop
      wine
      winetricks
      wtype
      xdragon
      zenity
    ];

    sessionVariables.WINEFSYNC = "1";

    pointerCursor = {
      inherit (pkgs.rice.cursorTheme) package name size;
      gtk.enable = true;
      x11.enable = true;
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
          inherit (config.home.sessionVariables) QT_QPA_PLATFORMTHEME;
          XCURSOR_PATH = "/run/host/user-share/icons:/run/host/share/icons";
        };
        Context.filesystems = [
          "/etc/profiles/per-user/bandithedoge/bin:ro"
          "/nix/store:ro"
          "/run/current-system/sw/bin:ro"
          "xdg-config/MangoHud:ro"
          "xdg-config/gtk-3.0:ro"
          "xdg-config/gtk-4.0:ro"
          "xdg-config/Kvantum:ro"
          "xdg-config/qt5ct:ro"
          "xdg-config/qt6ct:ro"
          "xdg-config/fontconfig:ro"
        ];
      };
      "org.jdownloader.JDownloader".Context.filesystems = [
        "/mnt"
      ];
    };
    packages = [
      "com.github.tchx84.Flatseal"
      "org.gtk.Gtk3theme.adw-gtk3-dark"
      "org.jdownloader.JDownloader"
    ];
  };
  # }}}

  systemd.user = {
    # HACK: https://github.com/nix-community/home-manager/issues/2659
    inherit (config.home) sessionVariables;
  };

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
    platformTheme.name = "qtct";
    style.package = with pkgs; [
      libsForQt5.qtstyleplugin-kvantum
      qt6Packages.qtstyleplugin-kvantum
    ];
  };

  xdg = {
    configFile = let
      css = pkgs.rice.compileSCSS ../../../gtk.scss;
      # qt {{{
      qtct = version:
        pkgs.lib.generators.toINI {} {
          Appearance = {
            color_scheme_path = "${config.xdg.configHome}/qt${builtins.toString version}ct/colors/rice.conf";
            custom_palette = true;
            icon_theme = "breeze-dark";
            standard_dialogs = "xdgdesktopportal";
            style = "kvantum-dark";
          };
          Fonts =
            if version == 6
            then {
              general = "\"${pkgs.rice.uiFont},11,-1,5,400,0,0,0,0,0,0,0,0,0,0,1\"";
              fixed = "\"${pkgs.rice.monoFont},11,-1,5,400,0,0,0,0,0,0,0,0,0,0,1\"";
            }
            else {
              general = "${pkgs.rice.uiFont},12,-1,5,50,0,0,0,0,0";
              fixed = "${pkgs.rice.monoFont},11,-1,5,50,0,0,0,0,0,Regular";
            };
          Interface = {
            activate_item_on_single_click = 0;
            buttonbox_layout = 0;
            cursor_flash_time = 1000;
            dialog_buttons_have_icons = 2;
            double_click_interval = 400;
            gui_effects = "@Invalid()";
            keyboard_scheme = 2;
            menus_have_icons = true;
            show_shortcuts_in_context_menus = true;
            stylesheets = "@Invalid()";
            toolbutton_style = 4;
            underline_shortcut = 1;
            wheel_scroll_lines = 3;
          };
          SettingsWindow.geometry = "@ByteArray(\\x1\\xd9\\xd0\\xcb\\0\\x3\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\x2\\xde\\0\\0\\x4\\b\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\x2\\xde\\0\\0\\x4\\b\\0\\0\\0\\0\\0\\0\\0\\0\\a\\x80\\0\\0\\0\\0\\0\\0\\0\\0\\0\\0\\x2\\xde\\0\\0\\x4\\b)";
          Troubleshooting = {
            force_raster_widgets = 1;
            ignored_applications = "@Invalid()";
          };
        };

      qtColorScheme = pkgs.lib.generators.toINI {} {
        ColorScheme = let
          concat = pkgs.lib.concatMapStringsSep "," (x: "#ff" + (pkgs.lib.removePrefix "#" x));
        in
          with pkgs.rice; rec {
            active_colors = concat [
              base05 # window text
              base00 # button bg
              base06 # bright
              base04 # less bright
              base00 # dark
              base01 # less dark
              base05 # normal text
              base06 # bright text
              base05 # button text
              base00 # normal bg
              base10 # window
              base00 # shadow
              base0F # highlight
              base00 # highlighted text
              base0F # link
              base0E # visited link
              base02 # alt bg
              base05 # default
              base02 # tooltip bg
              base05 # tooltip text
              base03 # placeholder text
              base0F # accent
            ];
            inactive_colors = active_colors;
            disabled_colors = concat [
              base03 # window text
              base00 # button bg
              base06 # bright
              base04 # less bright
              base00 # dark
              base01 # less dark
              base03 # normal text
              base06 # bright text
              base03 # button text
              base00 # normal bg
              base00 # window
              base00 # shadow
              base0F # highlight
              base00 # highlighted text
              base0F # link
              base0E # visited link
              base00 # alt bg
              base05 # default
              base02 # tooltip bg
              base05 # tooltip text
              base03 # placeholder text
              base0F # accent
            ];
          };
      };
      # }}}
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

      "qt6ct/qt6ct.conf".text = qtct 6;
      "qt6ct/colors/rice.conf".text = qtColorScheme;
      "qt5ct/qt5ct.conf".text = qtct 5;
      "qt5ct/colors/rice.conf".text = qtColorScheme;
      "Kvantum/KvLibadwaita".source = flake.inputs.kvlibadwaita + "/src/KvLibadwaita";
      "Kvantum/kvantum.kvconfig".text = lib.generators.toINI {} {General.theme = "KvLibadwaitaDark";};
    };

    portal.extraPortals = with pkgs; [xdg-desktop-portal-gtk];

    mimeApps = rec {
      enable = true;
      associations.added = defaultApplications;
      defaultApplications = {
        "application/pdf" = "org.pwmt.zathura.desktop";
        "image/vnd.djvu" = "org.pwmt.zathura.desktop";
      };
    };
  };

  programs = {
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
        hwdec = "vdpau";
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
      ];
    };
  };

  services = {
    syncthing.enable = true;
    systembus-notify.enable = true;
    gnome-keyring.enable = true;
  };

  # polkit {{{
  # systemd.user.services.polkit-gnome-authentication-agent-1 = {
  #   Unit = {
  #     Description = "polkit-gnome-authentication-agent-1";
  #     After = ["graphical-session.target"];
  #   };
  #
  #   Install = {
  #     WantedBy = ["graphical-session.target"];
  #     Wants = ["graphical-session.target"];
  #     After = ["graphical-session.target"];
  #   };
  #
  #   Service = {
  #     Type = "simple";
  #     ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
  #     Restart = "on-failure";
  #     RestartSec = 1;
  #     TimeoutStopSec = 10;
  #   };
  # };
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
