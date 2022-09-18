{
  pkgs,
  home-manager,
  config,
  ...
}: let
  rice = import ../../../rice.nix {inherit pkgs;};

  oi = pkgs.callPackage ./oi {};
in {
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

  xdg.configFile."broot/conf.toml".source = (pkgs.formats.toml {}).generate "broot-config" {
    modal = true;
    default_flags = "gh";
  };
}
