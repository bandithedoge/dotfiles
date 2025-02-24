{
  pkgs,
  config,
  ...
}: {
  imports = [
    ./xdg.nix
  ];

  home = {
    # {{{
    stateVersion = "25.05";
    activation.setupEtc = config.lib.dag.entryAfter ["writeBoundary"] ''
      /run/current-system/sw/bin/systemctl start --user sops-nix
    '';
    packages = with pkgs; [
      # {{{
      (sox.override {enableLame = true;})
      (writeScriptBin "nust" (builtins.readFile ../../justfile))
      age
      aria
      broot
      chafa
      comma
      fd
      ffmpeg
      file
      hactool
      home-manager
      hyperfine
      imagemagickBig
      innoextract
      jq
      just
      killall
      librespeed-cli
      niv
      nix-output-monitor
      nix-prefetch
      nurl
      ouch
      pandoc
      patchelf
      rclone
      ripgrep
      sops
      unar
      unzip
      wgcf
      wget
      xdg-ninja
      xdg-user-dirs
      yt-dlp
    ]; # }}}
    shellAliases =
      {
        s = "sudo";
        c = "clear";
        e = "exit";
        b = "bash -c";
        v = "nvim";
      }
      // pkgs.lib.optionalAttrs pkgs.stdenv.isDarwin {
        bi = "brew install";
        bu = "brew uninstall";
        bs = "brew search";
        bq = "brew list";
      };
    file.".face".source = ../../face.png;
  }; # }}}

  xdg.configFile = {
    "rice.json".text = builtins.toJSON (pkgs.lib.filterAttrsRecursive (_: v: !builtins.isFunction v) pkgs.rice);

    # broot {{{
    "broot/conf.toml".source = (pkgs.formats.toml {}).generate "broot-config" {
      modal = true;
      default_flags = "gh";
      skin = let
        color = c: "rgb(${pkgs.colors.conversions.hexToRGBString ", " (pkgs.lib.removePrefix "#" c)})";
      in
        with pkgs.rice; {
          # {{{
          default = "${color base05} ${color base00}";
          tree = "${color base03} none";
          file = "${color base05} none";
          directory = "${color base0D} none bold";
          exe = "${color base0B} none bold";
          link = "${color base0E} none";
          perm__ = "${color base02} none";
          perm_r = "${color base0B} none";
          perm_w = "${color base09} none";
          perm_x = "${color base0E} none";
          owner = "${color base0C} none";
          group = "${color base0A} none";
          count = "${color base03} none";
          dates = "${color base03} none";
          content_match = "${color base0F} none";
          git_branch = "${color base05} none";
          git_insertions = "${color base0B} none";
          git_deletions = "${color base08} none";
          git_status_current = "${color base05} none";
          git_status_modified = "${color base09} none";
          git_status_new = "${color base0B} none";
          git_status_ignored = "${color base03} none";
          git_status_conflicted = "${color base0E} none";
          git_status_other = "${color base0D} none";
          selected_line = "none ${color base10}";
          char_match = "none ${color base0A} bold";
          file_error = "${color base08} none";
          flag_label = "${color base03} none";
          flag_value = "${color base05} none";
          input = "${color base04} none";
          status_error = "${color base00} ${color base08}";
          status_job = "${color base00} ${color base0F}";
          status_normal = "${color base04} ${color base02}";
          status_italic = "${color base0F} ${color base02}";
          status_bold = "${color base0F} ${color base02} bold";
          status_code = "${color base03} ${color base02}";
          status_ellipsis = "${color base03} ${color base02}";
          purpose_normal = "${color base05} ${color base01}";
          purpose_italic = "${color base0F} ${color base01}";
          purpose_bold = "${color base0F} ${color base01} bold";
          purpose_ellipsis = "${color base03} ${color base01}";
          scrollbar_track = "${color base01} none";
          scrollbar_thumb = "${color base0F} none";
          help_paragraph = "${color base0F} ${color base02} bold";
          help_bold = "${color base0F} none bold";
          help_italic = "${color base0F} none";
          help_code = "${color base03} ${color base01}";
          help_headers = "${color base0F} none bold";
          help_table_border = "${color base03} none";
          preview_title = "${color base05} ${color base02} bold";
          preview = "${color base05} ${color base10}";
          preview_line_number = "${color base03} ${color base02}";
          preview_match = "none ${color base0F}";
          hex_null = "${color base03} none";
          hex_ascii_graphic = "${color base0C} none";
          hex_ascii_whitespace = "${color base0A} none";
          hex_ascii_other = "${color base0B} none";
          hex_non_ascii = "${color base08} none";
          staging_area_title = "${color base0F} ${color base02} bold";
          mode_command_mark = "${color base00} ${color base0F} bold";
        }; # }}}
      verbs = [
        {
          invocation = "edit";
          key = "e";
          execution = "$EDITOR {file}";
          leave_broot = false;
        }
        {
          invocation = "dragon";
          external = "dragon {file}";
          leave_broot = false;
        }
        {
          key = "q";
          execution = ":quit";
        }
        {
          key = "p";
          execution = ":toggle_preview";
        }
        {
          key = "ctrl-h";
          execution = ":panel_left";
        }
        {
          key = "ctrl-l";
          execution = ":panel_right";
        }
        {
          key = ".";
          execution = ":toggle_hidden";
        }
      ];
    }; #}}}
  };

  programs = {
    # {{{
    home-manager = {
      enable = true;
    };

    fish = {
      # {{{
      enable = true;
      plugins =
        builtins.map (x: {
          name = x.pname;
          inherit (x) src;
        }) (with pkgs.fishPlugins; [
          puffer
          forgit
          autopair
          fzf-fish
          colored-man-pages
        ]);
      functions = let
        mkNom = subc: {
          body = ''
            nix --log-format internal-json -v ${subc} $argv &| nom --json
          '';
        };
      in {
        br.body = ''
          set f (mktemp)
          broot --outcmd $f $argv
          if test $status -ne 0
            rm -f "$f"
            return "$code"
          end
          set d (cat "$f")
          rm -f "$f"
          eval "$d"
        '';
        nb = mkNom "build";
        nd = mkNom "develop";
        nr = mkNom "run";
        ns = mkNom "shell";
      };
      interactiveShellInit = with pkgs.rice; ''
        set fish_color_autosuggestion '${base03}'
        set fish_color_cancel -r
        set fish_color_command green #white
        set fish_color_comment '${base03}'
        set fish_color_cwd green
        set fish_color_cwd_root red
        set fish_color_end brblack #blue
        set fish_color_error red --bold
        set fish_color_escape yellow #green
        set fish_color_history_current --bold
        set fish_color_host normal
        set fish_color_match --background=brblue
        set fish_color_normal normal
        set fish_color_operator blue #green
        set fish_color_param '${base04}'
        set fish_color_quote yellow #brblack
        set fish_color_redirection cyan
        set fish_color_search_match bryellow --background='${base02}'
        set fish_color_selection white --bold --background='${base02}'
        set fish_color_status red
        set fish_color_user brgreen
        set fish_color_valid_path --underline
        set fish_pager_color_completion normal
        set fish_pager_color_description yellow --dim
        set fish_pager_color_prefix white --bold #--underline
        set fish_pager_color_progress brwhite --background='${base02}'

        set fish_cursor_default block
        set fish_cursor_insert line
        set fish_cursor_replace_one underscore
        set fish_cursor_visual block

        fish_vi_key_bindings

        set fish_greeting

        set -Ux fifc_editor $EDITOR

        batman --export-env | source
      '';
    };
    # }}}

    starship = {
      # {{{
      enable = true;
      settings = {
        time = {
          disabled = false;
          format = "[$time]($style) ";
        };
        status = {
          disabled = false;
          format = "[$common_meaning$signal_name $status]($style) ";
        };
        git_metrics.disabled = false;

        bun.symbol = " ";
        c.symbol = " ";
        cmake.symbol = "󰔷 ";
        container.symbol = "󰋘 ";
        dart.symbol = " ";
        deno.symbol = " ";
        directory.read_only = " 󰌾";
        fennel.symbol = " ";
        git_branch.symbol = " ";
        git_commit.tag_symbol = "󰓹 ";
        golang.symbol = "";
        haskell.symbol = " ";
        hostname.ssh_symbol = " ";
        julia.symbol = " ";
        lua.symbol = "";
        meson.symbol = "󰔷 ";
        nim.symbol = "󰆥 ";
        nix_shell.symbol = " ";
        nodejs.symbol = " ";
        package.symbol = "󰏗 ";
        python.symbol = " ";
        ruby.symbol = " ";
        rust.symbol = " ";
        zig.symbol = " ";
      };
    };
    # }}}

    bottom = {
      # {{{
      enable = true;
      settings = {
        flags.mem_as_value = true;
        colors = with pkgs.rice; {
          avg_cpu_color = base0F;
          highlighted_border_color = base0F;
          cursor_color = base0F;
          scroll_entry_bg_color = base0F;
          table_header_color = base03;
        };
        row = [
          {
            child = [
              {
                type = "mem";
                ratio = 1;
              }
              {
                type = "cpu";
                ratio = 2;
              }
            ];
          }
          {
            ratio = 2;
            child = [{type = "proc";}];
          }
        ];
      };
    };
    # }}}

    bat = {
      enable = true;
      config.theme = "base16";
      extraPackages = with pkgs.bat-extras; [batman];
    };

    eza = {
      enable = true;
      icons = "auto";
      git = true;
    };

    skim = {
      enable = true;
      enableFishIntegration = false;
      defaultOptions = let
        concat = pkgs.lib.concatMapAttrsStringSep "," (k: v: "${k}:${v}");
      in
        with pkgs.rice; [
          "--prompt='❯ '"
          # "--pointer='❯'"
          # "--marker='┃'"
          "--tabstop=4"
          # "--border=none"

          "--color=${concat {
            fg = base05;
            bg = base00;
            "fg+" = base05;
            "bg+" = base10;
            selected-bg = base02;
            hl = base0F;
            "hl+" = base0F;
            gutter = base00;
            prompt = base0F;
            pointer = base0F;
            disabled = base03;
            marker = base0F;
            info = base0A;
            scrollbar = base0F;
            spinner = base0F;
            border = base0F;
          }}"

          "--bind=${concat {
            ctrl-h = "preview-up";
            ctrl-l = "preview-down";
            ctrl-d = "half-page-down";
            ctrl-u = "half-page-up";
          }}"
        ];
    };

    fzf = {
      enable = true;
      enableFishIntegration = false;
      inherit (config.programs.skim) defaultOptions;
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    git = {
      enable = true;
      package = pkgs.gitAndTools.gitFull;
      userName = "bandithedoge";
      userEmail = "bandithedoge@protonmail.com";
      ignores = ["*~"];
      lfs.enable = true;
      delta = {
        enable = true;
        options = {
          features = "rice";
          rice = with pkgs.rice; rec {
            dark = true;
            line-numbers = true;
            # side-by-side = true;

            file-style = base0F;
            plus-style = "${base00} ${base0B}";
            plus-non-emph-style = plus-style;
            plus-emph-style = "${base00} bold ul ${base0B}";
            plus-empty-line-marker-style = plus-style;
            minus-style = "${base00} ${base08}";
            minus-non-emph-style = minus-style;
            minus-emph-style = "${base00} bold ul ${base08}";
            minus-empty-line-marker-style = minus-style;
            line-numbers-left-style = base03;
            line-numbers-right-style = line-numbers-left-style;
            line-numbers-minus-style = base08;
            line-numbers-plus-style = base0B;
            line-numbers-zero-style = line-numbers-left-style;
            syntax-theme = "base16";
          };
        };
      };
    };

    yazi = {
      # {{{
      enable = true;
      enableFishIntegration = true;
      shellWrapperName = "y";
      package = pkgs.yazi.override {extraPackages = with pkgs; [glow miller hexyl exiftool mediainfo ouch clipboard-jh p7zip];};

      settings = {
        manager = {
          sort_by = "natural";
          show_hidden = true;
          linemode = "size";
        };
        preview = {
          wrap = "yes";
          tab_size = 4;
        };
        opener = {
          extract = [
            {
              run = "unar %*";
              desc = "Extract";
            }
          ];
        };
        plugin = {
          prepend_previewers = [
            {
              name = "*.md";
              run = "glow";
            }
            {
              mime = "text/csv";
              run = "miller";
            }
            {
              mime = "audio/*";
              run = "exifaudio";
            }
            {
              mime = "application/x-{tar,bzip2,7z-compressed,rar,xz}";
              run = "ouch";
            }
          ];
          append_previewers = [
            {
              name = "*";
              run = "hexyl";
            }
          ];
        };
      };

      plugins = pkgs.lib.genAttrs [
        "exifaudio"
        "glow"
        "hexyl"
        "miller"
        "ouch"
        "system-clipboard"
        "yatline"
      ] (name: pkgs.bandithedoge.yaziPlugins.${name});

      keymap = {
        manager.prepend_keymap = [
          {
            on = "q";
            run = "quit --no-cwd-file";
            desc = "Quit";
          }
          {
            on = "Q";
            run = "quit";
            desc = "Quit and cd";
          }
          {
            on = "C";
            run = "plugin ouch --args=zip";
            desc = "Compress with ouch";
          }
          {
            on = "E";
            run = "plugin eza-preview";
            desc = "Toggle tree/list dir preview";
          }
          {
            on = "<C-y>";
            run = "plugin system-clipboard";
            desc = "Copy file";
          }
          {
            on = "<C-n>";
            run = ''
              shell 'dragon "$@"' --confirm
            '';
            desc = "Drag and drop";
          }
          {
            on = "<Tab>";
            run = "tab_switch 1 --relative";
          }
          {
            on = "<S-Tab>";
            run = "tab_switch -1 --relative";
          }
        ];
        completion.prepend_keymap = [
          {
            on = "<C-j>";
            run = "arrow 1";
          }
          {
            on = "<C-k>";
            run = "arrow -1";
          }
        ];
      };

      theme = with pkgs.rice; {
        manager.border.fg = base03;
      };

      initLua = builtins.readFile (pkgs.runCommand "yazi/init.lua" {nativeBuildInputs = with pkgs; [fennel];}
        ''
          cat <<EOF > $out
            ${pkgs.rice.def.lua}
          EOF
          fennel -c ${./yazi.fnl} >> $out
        '');
    };
    # }}}

    fastfetch = {
      # {{{
      enable = true;
      settings = {
        display = {
          key.type = "both";
          size.binaryPrefix = "si";
        };
        modules = [
          "title"

          "break"

          "os"
          "kernel"
          "shell"
          "editor"
          "de"
          "wm"
          "wmtheme"
          "theme"
          "icons"
          "font"
          "terminal"
          "terminalfont"

          "break"

          "host"
          "board"
          "cpu"
          "gpu"
          "memory"

          "break"

          "colors"
        ];
      };
    };
    # }}}

    lazygit = {
      enable = true;
      settings = {
        gui = {
          theme = with pkgs.rice; {
            activeBorderColor = [base0F "bold"];
            inactiveBorderColor = [base03];
            searchingActiveBorderColor = [base0C];
            optionsTextColor = [base03];
            selectedLineBgColor = [base0F "bold"];
            inactiveViewSelectedLineBgColor = [base02];
            cherryPickedCommitFgColor = [base00];
            cherryPickedCommitBgColor = [base0C];
            markedBaseCommitFgColor = [base00];
            markedBaseCommitBgColor = [base0D];
            unstagedChangesColor = [base08];
            defaultFgColor = [base05];
          };
          showNumstatInFilesView = true;
          nerdFontsVersion = "3";
          showBranchCommitHash = true;
          showDivergenceFromBaseBranch = "arrowAndNumber";
          border = "single";
          spinner.frames = ["⣾" "⣽" "⣻" "⢿" "⡿" "⣟" "⣯" "⣷"];
        };
        # git.paging.useConfig = true;
        update.method = "never";
      };
    };

    dircolors.enable = true;
    gh.enable = true;
    git-credential-oauth.enable = true;
    info.enable = true;
    nh.enable = true;
    nix-index.enable = true;
    # }}}
  };

  manual = {
    manpages.enable = false;
    html.enable = true;
  };
}
