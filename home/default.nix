{ pkgs, home-manager, nix-colors, ... }:

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
    rebuild
    update
    fd
    neofetch
    clang
    nixfmt
    hactool
    rclone
    imagemagick
    mpc_cli
    ncdu
    ruby_3_0
    stylua
    nodejs
    nim
    unar
    tree
    librespeed-cli
    htop
    gh
    pistol
  ];

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
          "gnome_shell_extensions"
          "git_repos"
          "vim"
          "nix"
          "system"
          "brew_cask"
          "brew_formula"
        ];
        commands = { "Nix" = "${rebuild}"; };
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
      defaultOptions = [ "--height 70%" "--prompt '❯ '" ];
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

    kitty = {
      enable = true;
      font = {
        name = rice.monoFont;
        size = 16;
      };
      settings = {
        cursor_shape = "beam";
        enable_audio_bell = false;
        disable_ligatures = "cursor";
        window_padding_width = 10;
        adjust_column_width = -1;

        macos_titlebar_color = "background";
        macos_thicken_font = "0.25";

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
  };

}
