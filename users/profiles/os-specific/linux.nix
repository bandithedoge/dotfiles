{
  config,
  pkgs,
  ...
}: let
  rice = import ../../../rice.nix {inherit pkgs;};
in {
  home = {
    packages = with pkgs; [
      cutter
      dfeet
      discord-canary
      ghidra
      gparted
      icon-library
      imv
      keepassxc
      oomoxFull
      pavucontrol
      pcmanfm
      teams
      tigervnc
      wine
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

  programs.chromium = {
    # {{{
    enable = true;
    package = pkgs.vivaldi.override {proprietaryCodecs = true;};
    extensions =
      builtins.map
      (id: {inherit id;})
      (
        builtins.attrValues {
          "Augmented Steam" = "dnhpnfgdlenaccegplpojghhmaamnnfp";
          "Base64 encode/decode selected text" = "gkdcpimagggbnjdkjhbnilfeiidhdhcl";
          "BetterViewer" = "llcpfkbjgkpmapiidpnohffjmmnhpmpb";
          "Canvas Blocker for Google Chrome" = "einlodpanajlpficpkejcoajkadbddjn";
          "CSGOFloat Market Checker" = "jjicbefpemnphinccgikpdaagjebbnhg";
          "Don't F*** With Paste" = "nkgllhigpcljnhoakjkgaieabnkmgdkb";
          "Enhanced GitHub" = "anlikcnbgdeidpacdbdljnabclhahhmd";
          "Enhancer for YouTube" = "ponfpcnoihfmfllpaingbgckeeldkhle";
          "Gitako - GitHub file tree" = "giljefjcheohhamkjphiebfjnlphnokk";
          "GitHub Code Folding" = "lefcpjbffalgdcdgidjdnmabfenecjdf";
          "GitHub Isometric Contributions" = "mjoedlfflcchnleknnceiplgaeoegien";
          "GitHub Repository Size" = "apnjnioapinblneaedefcnopcjepgkci";
          "Imagus" = "immpkjjlgappgfkkfieppnmlhakdmaab";
          "Lovely forks" = "ialbpcipalajnakfondkflpkagbkdoib";
          "Material Icons for GitHub" = "bggfcpfjbdkhfhfmkjpbhnkhnpjjeomc";
          "npmhub" = "kbbbjimdjbjclaebffknlabpogocablj";
          "OctoLinker" = "jlmafbaeoofdegohdhinkhilhclaklkp";
          "Privacy Badger" = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp";
          "Privacy Pass" = "ajhmfdgkijocedmfjonnpjfojldioehi";
          "PronounDB" = "nblkbiljcjfemkfjnhoobnojjgjdmknf";
          "Reddit Enhancement Suite" = "kbmfpngjjgdllneeigpgjifpgocmfgmb";
          "Refined GitHub" = "hlepfoohegkhhmjieoechaddaejaokhf";
          "Ruffle" = "donbcfbmhbcapadipfkeojnmajbakjdc";
          "ScrollAnywhere" = "jehmdpemhgfgjblpkilmeoafmkhbckhi";
          "Sourcegraph" = "dgjhfomjieaadpoljlnidmbgkdffpack";
          "SponsorBlock for YouTube" = "mnjggcdmjocbbbhaepdhchncahnbgone";
          "SteamDB" = "kdbmhfkmnlmbkgbabkdealhhbfhlmmon";
          "Stylus" = "clngdbkpkpeebahjckkjfobafhncgmne";
          "Vimium C" = "hfjbmagddngcpeloejdejnfgbamkjaeg";
          "Violentmonkey" = "jinjaccalgkegednnccohejagnlnfdag";
        }
      );
  };
  # }}}

  programs.alacritty = {
    # {{{
    enable = true;
    settings = {
      window = {
        decorations = "none";
        padding = {
          x = 10;
          y = 10;
        };
        dynamic_padding = true;
      };
      font = {
        normal.family = rice.monoFont;
        size = 10.5;
      };
      colors = with rice; {
        primary = {
          background = base00;
          foreground = base05;
        };
        cursor = {
          text = base00;
          cursor = base0F;
        };
        vi_mode_cursor = {
          text = base00;
          cursor = base05;
        };
        search = {
          matches = {
            foreground = base00;
            background = base0A;
          };
          focused_match = {
            foreground = base00;
            background = base0F;
          };
        };
        hints = {
          start = {
            foreground = base0F;
            background = base02;
          };
          end = {
            foreground = base03;
            background = base02;
          };
        };
        line_indicator = {
          background = base10;
        };
        footer_bar = {
          foreground = base0F;
          background = base10;
        };
        selection = {
          text = base00;
          background = base05;
        };
        normal = {
          black = base01;
          red = base08;
          green = base0B;
          yellow = base0A;
          blue = base0D;
          magenta = base0E;
          cyan = base0C;
          white = base06;
        };
        bright = {
          black = base02;
          red = base12;
          green = base14;
          yellow = base13;
          blue = base16;
          magenta = base17;
          cyan = base15;
          white = base0F;
        };
      };
      cursor = {
        style = {
          shape = "Beam";
          blinking = "Always";
        };
        vi_mode_style = "Block";
        thickness = 0.25;
      };
    };
  };
  # }}}

  programs.firefox = {
    enable = true;
    package = pkgs.firefox-beta-bin.override {
      cfg.enableTridactylNative = true;
    };
    extensions = with pkgs.bandithedoge.firefoxAddons; [
      augmented-steam
      base64-decoder
      betterviewer
      canvasblocker
      csgofloat
      dont-fuck-with-paste
      enhanced-github
      enhancer-for-youtube
      gitako
      github-code-folding
      github-isometric-contributions
      github-repo-size
      imagus
      lovely-forks
      material-icons-for-github
      npm-hub
      octolinker
      privacy-badger17
      privacy-pass
      pronoundb
      reddit-enhancement-suite
      refined-github
      ruffle_rs
      sourcegraph-for-firefox
      sponsorblock
      steam-database
      stylus
      tridactyl-vim
      violentmonkey
    ];
    profiles = {
      default = {
        settings = {};
      };
    };
  };

  xdg.configFile."vivaldi/css/vivaldi.css".source = let
    input = pkgs.writeText "vivaldi.scss" ''
      ${rice.def.scss}

      ${builtins.readFile ./vivaldi.scss}
    '';
  in
    pkgs.runCommand "vivaldi.css" {} ''
      ${pkgs.sass}/bin/sass ${input} $out
    '';

  xdg.configFile."discordcanary/settings.json".text = builtins.toJSON {
    enableHardwareAcceleration = false;
    OPEN_ON_STARTUP = false;
    openasar = {
      js = let
        plugins =
          builtins.concatStringsSep "\n"
          (builtins.map (url: "cumcord.plugins.importPlugin('${url}');") [
            "https://cumcordplugins.github.io/Condom/cumcord.xirreal.dev/vcTimer"
            "https://cumcordplugins.github.io/Condom/yellowsink.github.io/c7-cc-plugs/MessageLinkPreview"
            "https://cumcordplugins.github.io/Condom/yellowsink.github.io/c7-cc-plugs/PlatformIcons"
            "https://cumcordplugins.github.io/Condom/yellowsink.github.io/c7-cc-plugs/ChannelTypingIndicator"
            "https://cumcordplugins.github.io/Condom/e-boi.github.io/cumcord-plugins/betterfriendslist/dist"
            "https://cumcordplugins.github.io/Condom/yellowsink.github.io/cc-plugins/svg-embeds"
            "https://cumcordplugins.github.io/Condom/20kdc.gitlab.io/kdc-cord-plugins/gcat"
            "https://cumcordplugins.github.io/Condom/yellowsink.github.io/cc-plugins/cumstain"
            "https://cumcordplugins.github.io/Condom/swishs-client-mod-plugins.github.io/cumcord-plugins/plugins/permission-viewer"
            "https://cumcordplugins.github.io/Condom/skullyplugs.github.io/cc-plugins/extended-timestamps"
            "https://cumcordplugins.github.io/Condom/e-boi.github.io/cumcord-plugins/showconnections/dist"
            "https://cumcordplugins.github.io/Condom/yellowsink.github.io/cc-plugins/codeblocks-plus"
            "https://cumcordplugins.github.io/Condom/yellowsink.github.io/cc-plugins/who-reacted"
            "https://cumcordplugins.github.io/Condom/e-boi.github.io/cumcord-plugins/github-in-discord/dist"
          ]);
      in ''
        async function loadPlugins() {
          await cumcord.cum();

          ${plugins}
        }

        fetch('https://raw.githubusercontent.com/Cumcord/builds/main/build.js').then(r=>r.text()).then(eval).then(loadPlugins);
      '';
      quickstart = false;
      setup = true;
    };
  };

  services.syncthing.enable = true;
}
