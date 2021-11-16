{ pkgs, home-manager, inputs, ... }:

{
  imports = [ ./neovim ];
  home.sessionVariables = { EDITOR = "nvim"; };

  home.packages = with pkgs; [
    fd
    neofetch
    clang_12
    nix-du
    nixfmt
    hactool
    rclone
    imagemagick
    mpc_cli
    ncdu
    ruby_3_0
    stylua
    nodejs
    yarn
    wrangler
    hub
    nix-update
    smartmontools
  ];

  programs = {
    home-manager = {
      enable = true;
      path = "...";
    };

    topgrade = {
      enable = true;
      settings = {
        assume_yes = true;
        set_title = true;
        cleanup = true;
        brew.greedy_cask = true;
        disable = [ "gnome_shell_extensions" "git_repos" "vim" "nix" ];
        commands = {
          "Nix" =
            "nix-channel --update; darwin-rebuild switch; nix-collect-garbage --delete-older-than 7d";
        };
      };
    };

    lf = {
      enable = true;
      settings = {
        ignorecase = true;
        icons = true;
        hidden = true;
        wrapscroll = true;
      };
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

    # git {{{
    git = {
      enable = true;
      userName = "bandithedoge";
      userEmail = "bandithedoge@protonmail.com";
    };
    lazygit.enable = true;
    gh.enable = true;
    # }}}

    # shell {{{
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
      defaultOptions = [ "--height 70%" "--prompt ❯" ];
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
  };
}
