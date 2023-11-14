{pkgs, ...}: let
  oi = pkgs.callPackage ./oi {};
in {
  imports = [./xdg.nix];

  inherit (import ../../nix.nix {inherit pkgs;}) nix;

  manual.manpages.enable = false;

  nixpkgs.config = {allowBroken = true;};

  home = {
    # {{{
    stateVersion = "23.05";
    sessionPath = ["$HOME/.pub-cache/bin"];
    sessionVariables = {
      EDITOR = "nvim";
      MANROFFOPT = "-c";
      MANPAGER = "sh -c 'col -bx | ${pkgs.lib.getExe pkgs.bat} -l man -p'";
    };
    packages = with pkgs; [
      # {{{
      age
      aria
      broot
      comma
      fd
      ffmpeg
      file
      gh
      git
      hactool
      htop
      imagemagick
      jq
      killall
      librespeed-cli
      luajit
      manix
      ncdu
      neofetch
      nethack
      niv
      nix-prefetch
      oi
      ouch
      pandoc
      rclone
      ripgrep
      sox
      tree
      unzip
      vimiv-qt
      wgcf
      wget
      xdg-ninja
      xdragon
      yt-dlp
    ];
    # }}}
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
  };
  # }}}

  xdg.configFile."rice.json".text = builtins.toJSON (pkgs.lib.filterAttrsRecursive (_: v: !builtins.isFunction v) pkgs.rice);

  programs.home-manager = {
    enable = true;
    path = "...";
  };

  programs.fish = {
    # {{{
    enable = true;
    functions = {
      br = {
        body = ''
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
      };
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
    '';
  };
  # }}}

  programs.starship = {
    # {{{
    enable = true;
    settings = {
      golang.symbol = "";
      lua.symbol = "";
      nim.symbol = "";
      nix_shell.symbol = "";
      python.symbol = "";
      ruby.symbol = "";
      rust.symbol = "";
      time = {
        disabled = false;
        format = "[$time]($style) ";
      };
      status = {
        disabled = false;
        format = "[$common_meaning$signal_name $status]($style) ";
      };
      git_metrics.disabled = false;
      directory.read_only = "";
    };
  };
  # }}}

  programs.bottom = {
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

  programs.lazygit.enable = true;

  programs.gitui = {
    # {{{
    enable = false;
    keyConfig =
      # {{{
      # copied from https://github.com/extrawurst/gitui/blob/master/vim_style_key_config.ron
      ''
        (
          open_help: Some(( code: F(1), modifiers: ( bits: 0,),)),

          move_left: Some(( code: Char('h'), modifiers: ( bits: 0,),)),
          move_right: Some(( code: Char('l'), modifiers: ( bits: 0,),)),
          move_up: Some(( code: Char('k'), modifiers: ( bits: 0,),)),
          move_down: Some(( code: Char('j'), modifiers: ( bits: 0,),)),

          popup_up: Some(( code: Char('p'), modifiers: ( bits: 2,),)),
          popup_down: Some(( code: Char('n'), modifiers: ( bits: 2,),)),
          page_up: Some(( code: Char('b'), modifiers: ( bits: 2,),)),
          page_down: Some(( code: Char('f'), modifiers: ( bits: 2,),)),
          home: Some(( code: Char('g'), modifiers: ( bits: 0,),)),
          end: Some(( code: Char('G'), modifiers: ( bits: 1,),)),
          shift_up: Some(( code: Char('K'), modifiers: ( bits: 1,),)),
          shift_down: Some(( code: Char('J'), modifiers: ( bits: 1,),)),

          edit_file: Some(( code: Char('I'), modifiers: ( bits: 1,),)),

          status_reset_item: Some(( code: Char('U'), modifiers: ( bits: 1,),)),

          diff_reset_lines: Some(( code: Char('u'), modifiers: ( bits: 0,),)),
          diff_stage_lines: Some(( code: Char('s'), modifiers: ( bits: 0,),)),

          stashing_save: Some(( code: Char('w'), modifiers: ( bits: 0,),)),
          stashing_toggle_index: Some(( code: Char('m'), modifiers: ( bits: 0,),)),

          stash_open: Some(( code: Char('l'), modifiers: ( bits: 0,),)),

          abort_merge: Some(( code: Char('M'), modifiers: ( bits: 1,),)),
        )
      '';
    # }}}
    theme = let
      color = c: "Some(Rgb(${pkgs.colors.conversions.hexToRGBString "," (pkgs.lib.removePrefix "#" c)}))";
    in
      with pkgs.rice;
      # {{{
        ''
          (
            selected_tab: ${color base0F},
            command_fg: ${color base05},
            selection_bg: ${color base0F},
            selection_fg: ${color base00},
            cmdbar_bg: ${color base02},
            cmdbar_extra_lines_bg: ${color base01},
            disabled_fg: ${color base03},
            diff_line_add: ${color base0B},
            diff_line_delete: ${color base08},
            diff_file_added: ${color base14},
            diff_file_removed: ${color base12},
            diff_file_moved: ${color base17},
            diff_file_modified: ${color base13},
            commit_hash: ${color base0E},
            commit_time: ${color base15},
            commit_author: ${color base0B},
            danger_fg: ${color base08},
            push_gauge_bg: ${color base02},
            push_gauge_fg: ${color base04},
            tag_fg: ${color base17},
            branch_fg: ${color base13},
          )
        '';
    # }}}
  };
  # }}}

  programs.tealdeer = {
    enable = true;
    settings.display.compact = true;
  };

  programs.bat = {
    enable = true;
    config.theme = "base16";
  };

  programs.eza = {
    enable = true;
    enableAliases = true;
    icons = true;
    git = true;
  };

  programs.skim = {
    enable = true;
    enableFishIntegration = true;
    defaultOptions = with pkgs.rice; [
      "--prompt='❯ '"
      "--color=bg+:${base02},bg:${base00},spinner:${base0F},hl:${base0F},fg:${base04},header:${base0F},info:${base0A},pointer:${base0F},marker:${base0C},fg+:${base06},prompt:${base0F},hl+:${base0D}"
      "--tabstop=4"
      "--bind=ctrl-d:half-page-down,ctrl-u:half-page-up"
    ];
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.nix-index.enable = true;
  programs.dircolors.enable = true;
  programs.info.enable = true;

  # broot {{{
  xdg.configFile."broot/conf.toml".source = (pkgs.formats.toml {}).generate "broot-config" {
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
  }; # }}}
}
