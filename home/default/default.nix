{
  pkgs,
  config,
  ...
}:
{
  imports = [
    ./xdg.nix
  ];

  home = {
    # {{{
    stateVersion = "25.05";
    activation.setupEtc = config.lib.dag.entryAfter [ "writeBoundary" ] ''
      /run/current-system/sw/bin/systemctl start --user sops-nix
    '';
    packages = with pkgs; [
      # {{{
      (sox.override { enableLame = true; })
      (writeScriptBin "nust" (builtins.readFile ../../justfile))
      age
      aria
      comma
      fd
      ffmpeg
      file
      home-manager
      hyperfine
      imagemagickBig
      innoextract
      jq
      just
      killall
      librespeed-cli
      lix-diff
      ncdu
      niv
      nix-output-monitor
      nix-prefetch
      nurl
      ouch
      pandoc
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
    "rice.json".text = builtins.toJSON (
      pkgs.lib.filterAttrsRecursive (_: v: !builtins.isFunction v) pkgs.rice
    );
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
        builtins.map
          (x: {
            name = x.pname;
            inherit (x) src;
          })
          (
            with pkgs.fishPlugins;
            [
              autopair
              done
              forgit
              fzf-fish
              puffer
            ]
          );
      functions =
        let
          mkNom = subc: {
            body = ''
              nix --log-format internal-json -v ${subc} $argv &| nom --json
            '';
          };
        in
        {
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
            child = [ { type = "proc"; } ];
          }
        ];
      };
    };
    # }}}

    bat = {
      enable = true;
      config.theme = "base16";
      extraPackages = with pkgs.bat-extras; [ batman ];
    };

    eza = {
      enable = true;
      icons = "auto";
      git = true;
    };

    skim = {
      enable = true;
      enableFishIntegration = false;
      defaultOptions =
        let
          concat = pkgs.lib.concatMapAttrsStringSep "," (k: v: "${k}:${v}");
        in
        with pkgs.rice;
        [
          "--prompt='❯ '"
          # "--pointer='❯'"
          # "--marker='┃'"
          "--tabstop=4"
          # "--border=none"

          "--color=${
            concat {
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
            }
          }"

          "--bind=${
            concat {
              ctrl-h = "preview-up";
              ctrl-l = "preview-down";
              ctrl-d = "half-page-down";
              ctrl-u = "half-page-up";
            }
          }"
        ];
    };

    fzf = {
      enable = true;
      enableFishIntegration = false;
      inherit (config.programs.skim) defaultOptions;
    };

    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
        package = pkgs.lixPackageSets.latest.nix-direnv;
      };
    };

    git = {
      enable = true;
      package = pkgs.gitAndTools.gitFull;
      userName = "bandithedoge";
      userEmail = "bandithedoge@protonmail.com";
      ignores = [ "*~" ];
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
      package = pkgs.yazi.override {
        extraPackages = with pkgs; [
          clipboard-jh
          exiftool
          glow
          hexyl
          mediainfo
          ouch
          p7zip
          ripdrag
        ];
      };

      settings = {
        mgr = {
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
              run = "unar \"$@\"";
              desc = "Extract";
            }
          ];
        };
        open.append_rules = [
          {
            name = "*.zip";
            use = "extract";
          }
        ];
        plugin = {
          prepend_previewers = [
            {
              name = "*.md";
              run = "piper -- CLICOLOR_FORCE=1 glow -w=$w -s=dark \"$1\"";
            }
            {
              mime = "audio/*";
              run = "piper -- exiftool -q -q -S -Duration -SampleRate -AudioSampleRate -AudioBitrate -AvgBitrate -Channels -AudioChannels \"$1\"";
            }
            {
              mime = "application/x-{tar,7z-*,rar,xz}";
              run = "piper -- ouch list -t \"$1\"";
            }
            {
              mime = "application/{*zip*,rar,7z*,xz}";
              run = "piper -- ouch list -t \"$1\"";
            }
            {
              name = "*.{zip,rar}";
              run = "piper -- ouch list -t \"$1\"";
            }
            {
              name = "*.txt.gz";
              run = "piper -- zless \"$1\"";
            }
          ];
          append_previewers = [
            {
              name = "*";
              run = "piper -- hexyl --border=none --terminal-width=$w \"$1\"";
            }
          ];
          prepend_fetchers = [
            {
              id = "git";
              name = "*";
              run = "git";
            }
            {
              id = "git";
              name = "*/";
              run = "git";
            }
          ];
        };
      };

      plugins = pkgs.lib.genAttrs [
        "git"
        "lazygit"
        "mount"
        "piper"
        "smart-enter"
        "system-clipboard"
        "yatline"
      ] (name: pkgs.yaziPlugins.${name});

      keymap = {
        mgr.prepend_keymap = [
          {
            on = "l";
            run = "plugin smart-enter";
            desc = "Enter directory or open file";
          }
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
            on = "<C-y>";
            run = "plugin system-clipboard";
            desc = "Copy file";
          }
          {
            on = "<C-n>";
            run = "shell -- ripdrag -n \"$@\"";
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
          {
            on = [
              "g"
              "i"
            ];
            run = "plugin lazygit";
            desc = "Open lazygit";
          }
          {
            on = "M";
            run = "plugin mount";
            desc = "Mount drives";
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
        mgr.border.fg = base03;
      };

      initLua = builtins.readFile (
        pkgs.runCommand "yazi/init.lua" { nativeBuildInputs = with pkgs; [ fennel ]; } ''
          cat <<EOF > $out
            ${pkgs.rice.def.lua}
          EOF
          fennel -c ${./yazi.fnl} >> $out
        ''
      );
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
            activeBorderColor = [
              base0F
              "bold"
            ];
            inactiveBorderColor = [ base03 ];
            searchingActiveBorderColor = [ base0C ];
            optionsTextColor = [ base03 ];
            selectedLineBgColor = [
              base0F
              "bold"
            ];
            inactiveViewSelectedLineBgColor = [ base02 ];
            cherryPickedCommitFgColor = [ base00 ];
            cherryPickedCommitBgColor = [ base0C ];
            markedBaseCommitFgColor = [ base00 ];
            markedBaseCommitBgColor = [ base0D ];
            unstagedChangesColor = [ base08 ];
            defaultFgColor = [ base05 ];
          };
          showNumstatInFilesView = true;
          nerdFontsVersion = "3";
          showBranchCommitHash = true;
          showDivergenceFromBaseBranch = "arrowAndNumber";
          border = "single";
          spinner.frames = [
            "⣾"
            "⣽"
            "⣻"
            "⢿"
            "⡿"
            "⣟"
            "⣯"
            "⣷"
          ];
        };
        # git.paging.useConfig = true;
        update.method = "never";
      };
    };

    gh = {
      enable = true;
      settings.git_protocol = "ssh";
    };

    jujutsu.enable = true;
    dircolors.enable = true;
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
