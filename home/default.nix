{ pkgs, home-manager, ... }:

let

  rice = import ../rice.nix;

  rebuild = pkgs.writeShellScriptBin "rebuild" ''
    #!/usr/bin/env bash

    ${
      if pkgs.stdenv.isDarwin then "darwin-rebuild" else "sudo nixos-rebuild"
    } switch --flake ~/dotfiles --impure -v
  '';

  update = pkgs.writeShellScriptBin "update" ''
    sudo nix-collect-garbage
    nix flake update ~/dotfiles
    nix flake lock ~/dotfiles
    rebuild
    sudo nix-env -p /nix/var/nix/profiles/system --delete-generations +3
    nix-store --optimize
  '';

in {
  imports = [ ./editors ./garbage ./web ];

  manual.html.enable = true;

  home = {
    sessionVariables = {
      EDITOR = "nvim";
      BROWSER = "qutebrowser";
      LF_ICONS = "${builtins.readFile ./icons}";
      TERM = "xterm-256color";
    };
    packages = with pkgs; [
      clang
      fd
      gh
      hactool
      htop
      imagemagick
      librespeed-cli
      mpc_cli
      ncdu
      neofetch
      nix-diff
      nixfmt
      nodejs
      pandoc
      rclone
      rebuild
      ruby_3_0
      stylua
      tree
      unar
      update
    ];
    shellAliases = {
      s = "sudo";
      c = "clear";
      e = "exit";
      b = "bash -c";
      v = "nvim";

      ni = "nix-env -i";
      nr = "nix-env -e";
      ns = "nix-env -qas";
      nq = "nix-env -q";

      pi = "paru -S";
      pr = "paru -Rns";
      ps = "paru -Ss";
      pq = "paru -Qs";

      bi = "brew install";
      br = "brew uninstall";
      bs = "brew search";
      bq = "brew list";
    };
  };

  programs = {
    home-manager = {
      enable = true;
      path = "...";
    };

    # shell {{{
    lazygit.enable = true;

    topgrade = {
      enable = true;
      settings = {
        assume_yes = true;
        set_title = true;
        cleanup = true;
        brew.greedy_cask = true;
        disable = [
          "brew_cask"
          "brew_formula"
          "git_repos"
          "gnome_shell_extensions"
          "nix"
          "system"
          "vim"
        ];
        commands = { "Nix" = "${rebuild}"; };
      };
    };

    lf = rec {
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

    starship = {
      enable = true;
      settings = {
        golang.symbol = "";
        lua.symbol = "";
        nim.symbol = "";
        nix_shell.symbol = "";
        python.symbol = "";
        ruby.symbol = "";
        rust.symbol = "";
        time = { disabled = false; };
      };
    };
    bat.enable = true;

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

    fish = {
      enable = true;
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
        '' + builtins.readFile ./fish.fish;
    };
    # }}}

    # terminal {{{
    kitty = {
      enable = true;
      font = {
        name = rice.monoFont;
        size = if pkgs.stdenv.isDarwin then 16 else 12;
      };
      keybindings = { "ctrl+enter" = "no_op"; };
      settings = with rice; {
        term = "xterm-kitty";
        cursor_shape = "beam";
        enable_audio_bell = false;
        disable_ligatures = "cursor";
        window_padding_width = 10;
        adjust_column_width = -1;
        tab_bar_style = "powerline";

        macos_titlebar_color = "background";
        macos_thicken_font = "0.25";

        background = base00;
        foreground = base05;
        selection_background = base05;
        selection_foreground = base00;
        url_color = base0F;
        cursor = base05;
        active_border_color = base0F;
        inactive_border_color = base01;
        active_tab_background = base0F;
        active_tab_foreground = base00;
        inactive_tab_background = base02;
        inactive_tab_foreground = base05;

        color0 = base01;
        color1 = base08;
        color2 = base0B;
        color3 = base09;
        color4 = base0D;
        color5 = base0E;
        color6 = base0C;
        color7 = base06;

        color8 = base02;
        color9 = base12;
        color10 = base14;
        color11 = base13;
        color12 = base16;
        color13 = base17;
        color14 = base15;
        color15 = base0F;
      };
    };
    # }}}

    mpv = {
      enable = true;
      scripts = with pkgs.mpvScripts; [
        cutter
        thumbnail
        sponsorblock
        youtube-quality
      ];
    };

    zathura = {
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
  };

}
