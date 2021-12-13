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
  imports = [ ./neovim ];

  home.sessionVariables = {
    EDITOR = "nvim";
    LF_ICONS = "${builtins.readFile ./icons}";
  };
  home.packages = with pkgs; [
    clang
    fd
    gh
    hactool
    htop
    imagemagick
    keepassxc
    librespeed-cli
    mpc_cli
    ncdu
    neofetch
    nim
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

  manual.html.enable = true;

  programs = {
    home-manager = {
      enable = true;
      path = "...";
    };

    # text editors {{{
    vscode = {
      enable = true;
      package = pkgs.vscodium;
      extensions = with pkgs.vscode-extensions; [
        coenraads.bracket-pair-colorizer
        eamodio.gitlens
        mvllow.rose-pine
      ];
    };
    # }}}

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
      commands = { fzf = "$fd . -d1 | sk --preview ${previewer.source}"; };
      previewer.source = pkgs.writeShellScript "pv.sh" ''
        #!/usr/bin/env bash

        case "$1" in
          *.tar* | *.zip | *.7z | *.rar | *.iso | *.jar)
            ${pkgs.unar}/bin/lsar "$1" | tail -n +2 | tree -C --fromfile . ;;
          *) ${pkgs.pistol}/bin/pistol "$1" ;;
        esac
      '';
    };

    starship.enable = true;
    bat.enable = true;
    fzf.enable = true;
    tmux.enable = true;

    lsd = {
      enable = true;
      enableAliases = true;
    };

    skim = {
      enable = true;
      enableFishIntegration = true;
      defaultOptions = [ "--prompt '❯ '" ];
    };

    fish = {
      enable = true;
      interactiveShellInit = ''
        fish_vi_key_bindings
        set fish_greeting

        set TERM "xterm-256color"
        set PATH $PATH ~/.local/bin ~/.yarn/bin ~/.nimble/bin
        set fish_color_normal           '#d9dceb'
        set fish_color_command          '#69d26e'
        set fish_color_quote            '#d7953f'
        set fish_color_redirection      '#b96be1'
        set fish_color_end              '#e1c85c'
        set fish_color_error            '#eb585f'
        set fish_color_param            '#d9dceb'
        set fish_color_comment          '#474dab'

        set fish_color_autosuggestion   '#474dab'

        set fish_cursor_default     block
        set fish_cursor_insert      line
        set fish_cursor_replace_one underscore
        set fish_cursor_visual      block
      '';
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
      plugins = [
        # {
        #   name = "fenv";
        #   src = fetchGit {
        #     url = "https://github.com/oh-my-fish/plugin-foreign-env";
        #   };
        # }
        # {
        #   name = "autopair";
        #   src = fetchGit {
        #     url = "https://github.com/jorgebucaran/autopair.fish";
        #     ref = "main";
        #   };
        # }
      ];
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
      settings = {
        term = "xterm-kitty";
        cursor_shape = "beam";
        enable_audio_bell = false;
        disable_ligatures = "cursor";
        window_padding_width = 10;
        adjust_column_width = -1;

        macos_titlebar_color = "background";
        macos_thicken_font = "0.25";

        tab_bar_style = "powerline";
        active_tab_foreground = rice.accent1;
        active_tab_background = rice.accent0;
        inactive_tab_foreground = rice.comment;
        inactive_tab_background = rice.bg2;
        active_border_color = rice.accent;
        inactive_border_color = rice.bg2;

        foreground = rice.fg;
        background = rice.bg;
        selection_foreground = rice.fg;
        selection_background = rice.selection;
        cursor = rice.fg;
        cursor_text_color = "background";

        color0 = rice.bg2;
        color1 = rice.red;
        color2 = rice.green;
        color3 = rice.yellow;
        color4 = rice.blue;
        color5 = rice.purple;
        color6 = rice.cyan;
        color7 = rice.fg;

        color8 = rice.comment;
        color9 = rice.red1;
        color10 = rice.green1;
        color11 = rice.yellow1;
        color12 = rice.blue1;
        color13 = rice.purple1;
        color14 = rice.cyan1;
        color15 = rice.accent;
      };
    };
    # }}}

    # web browser {{{
    firefox = {
      enable = true;
      package = if pkgs.stdenv.isDarwin then
        pkgs.firefox-beta-bin
      else
        pkgs.firefox-unwrapped;
      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        auto-tab-discard
        betterttv
        buster-captcha-solver
        canvasblocker
        clearurls
        close-other-windows
        facebook-container
        gesturefy
        greasemonkey
        h264ify
        honey
        https-everywhere
        i-dont-care-about-cookies
        localcdn
        netflix-1080p
        octolinker
        octotree
        old-reddit-redirect
        polish-dictionary
        privacy-possum
        reddit-enhancement-suite
        refined-github
        sponsorblock
        stylus
        tabcenter-reborn
        terms-of-service-didnt-read
        translate-web-pages
        tridactyl
        ublock-origin
        unpaywall
        view-image
        violentmonkey
        wayback-machine
      ];
      profiles."main" = {
        name = "main";
        settings = {
          "app.update.auto" = false;
          "browser.autofocus" = false;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "layers.acceleration.force-enabled" = true;
          "gfx.webrender.all" = true;
          "svg.context-properties.content.enabled" = true;
        };
        userChrome = builtins.readFile (builtins.fetchurl
          "https://git.sr.ht/~ranmaru/ff-vertical-tabs/blob/master/userChrome.css");
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
      options = {
        page-padding = 10;
        show-hidden = true;

        font = rice.uiFont + " 12";
        default-bg = rice.bg;
        default-fg = rice.fg;
        inputbar-bg = rice.bg2;
        inputbar-fg = rice.fg;
        notification-bg = rice.bg2;
        notification-fg = rice.fg;
        notification-error-bg = rice.red0;
        notification-error-fg = rice.red1;
        notification-warning-bg = rice.orange0;
        notification-warning-fg = rice.orange1;
        statusbar-bg = rice.bg2;
        statusbar-fg = rice.comment;
        highlight-color = rice.selection;
        highlight-fg = rice.accent;
        highlight-active-color = rice.accent;
        completion-bg = rice.bg2;
        completion-fg = rice.fg;
        completion-highlight-fg = rice.accent1;
        completion-highlight-bg = rice.accent0;
        render-loading-bg = rice.bg2;
        render-loading-fg = rice.fg;
        index-bg = rice.bg2;
        index-fg = rice.fg;
        index-active-bg = rice.accent0;
        index-active-fg = rice.accent1;

        recolor-lightcolor = rice.bg;
        recolor-darkcolor = rice.fg;
      };
    };
  };

}
