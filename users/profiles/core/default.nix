{
  pkgs,
  home-manager,
  config,
  ...
}: let
  rice = import ../../../rice.nix {inherit pkgs;};

  oi = pkgs.callPackage ./oi {};
in {
  inherit (import ../../../nix.nix {inherit pkgs;}) nix;

  manual.html.enable = true;

  xdg = {
    enable = true;
    configFile."rice.json".text = builtins.toJSON rice;
  };

  nixpkgs.config = {allowBroken = true;};

  home = {
    # {{{
    stateVersion = "21.11";
    sessionVariables = {
      EDITOR = "nvim";
      LF_ICONS = "${builtins.readFile ./icons}";
      TERM = "xterm-256color";
      GO111MODULE = "on";
    };
    packages = with pkgs; [
      # {{{
      broot
      comma
      fd
      gh
      git
      hactool
      htop
      imagemagick
      jq
      killall
      librespeed-cli
      luajit
      ncdu
      neofetch
      nethack
      niv
      nix-prefetch
      oi
      pandoc
      rclone
      ripgrep
      tree
      unar
    ];
    # }}}
    shellAliases = {
      s = "sudo";
      c = "clear";
      e = "exit";
      b = "bash -c";
      v = "nvim";

      ni = "nix-env -i";
      nu = "nix-env -e";
      ns = "nix search nixpkgs";
      nq = "nix-env -q";

      bi = "brew install";
      bu = "brew uninstall";
      bs = "brew search";
      bq = "brew list";
    };
  };
  # }}}

  programs = {
    home-manager = {
      enable = true;
      path = "...";
    };

    fish = {
      # {{{
      enable = true;
      functions = {
        "br" = {
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
      interactiveShellInit = with rice;
        ''
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
        ''
        + builtins.readFile ./fish.fish;
    };
    # }}}

    lf = rec {
      # {{{
      enable = true;
      settings = {
        ignorecase = true;
        icons = true;
        hidden = true;
        wrapscroll = true;
      };
      previewer.source = pkgs.writeShellScript "pv.sh" ''
        #!/usr/bin/env bash

        case "$1" in
          *.tar* | *.zip | *.7z | *.rar | *.iso | *.jar)
            ${pkgs.unar}/bin/lsar "$1" | tail -n +2 | tree -C --fromfile . ;;
          *) ${pkgs.pistol}/bin/pistol "$1" ;;
        esac
      '';
      keybindings = {
        f = ''
          $lf -remote "send $id select '$(fd . -H -d1 -c always | sk --ansi || true)'"
        '';
        F = ''
          $lf -remote "send $id select '$(fd . -H -c always | sk --ansi || true)'"
        '';
        x = "cut";
        d = "delete";
        gG = "$lazygit -p $PWD";
      };
    };
    # }}}

    starship = {
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

    bat.enable = true;
    lazygit.enable = true;
    nix-index.enable = true;

    lsd = {
      enable = true;
      enableAliases = true;
    };

    skim = {
      enable = true;
      enableFishIntegration = true;
      defaultOptions = with rice; [
        "--prompt='❯ '"
        "--color=bg+:${base02},bg:${base00},spinner:${base0F},hl:${base0F},fg:${base04},header:${base0F},info:${base0A},pointer:${base0F},marker:${base0C},fg+:${base06},prompt:${base0F},hl+:${base0D}"
        "--tabstop=4"
        "--bind=ctrl-d:half-page-down,ctrl-u:half-page-up"
      ];
    };

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
  };

  # broot {{{
  xdg.configFile."broot/conf.toml".source = (pkgs.formats.toml {}).generate "broot-config" {
    modal = true;
    default_flags = "gh";
    skin = let
      color = c: "rgb(${pkgs.colors.conversions.hexToRGBString ", " (pkgs.lib.removePrefix "#" c)})";
    in
      with rice; {
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
        apply_to = "file";
        external = "$EDITOR {file}";
        leave_broot = false;
      }
      {
        key = "q";
        internal = ":quit";
      }
      {
        key = "p";
        internal = ":toggle_preview";
      }
      {
        key = "ctrl-h";
        internal = ":panel_left";
      }
      {
        key = "ctrl-l";
        internal = ":panel_right";
      }
    ];
  }; # }}}
}
