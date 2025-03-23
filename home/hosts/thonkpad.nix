{pkgs, ...}: {
  programs.qutebrowser = {
    # {{{
    enable = true;
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
        default_page = "https://www.startpage.com";
        open_base_url = true;
        start_pages = "https://www.startpage.com";
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
}
