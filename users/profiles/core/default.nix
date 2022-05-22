{
  pkgs,
  home-manager,
  config,
  ...
}: let
  isNixOS = pkgs.lib.strings.hasInfix "nixos" (builtins.readFile /etc/os-release);

  rice = import ../../../rice.nix;

  rebuild = pkgs.writeShellScriptBin "rebuild" ''
    #!/usr/bin/env bash

    ${
      if !isNixOS
      then "home-manager --extra-experimental-features 'nix-command flakes'"
      else if pkgs.stdenv.isDarwin
      then "darwin-rebuild"
      else "sudo nixos-rebuild"
    } \
      switch --flake ~/dotfiles${
      if !isNixOS
      then "#bandithedoge"
      else ""
    } --impure "$@" |& nom

  '';

  update = pkgs.writeShellScriptBin "update" ''
    nix flake update ~/dotfiles
    nix-channel --update
    rebuild
    sudo nix-collect-garbage
    nix flake lock ~/dotfiles
    sudo nix-env -p /nix/var/nix/profiles/system --delete-generations +3
    nix-store --optimize
  '';
in {
  manual.html.enable = true;

  xdg = {
    enable = true;
    # configHome = "${config.home.homeDirectory}/.config";
    configFile."rice.json".text = builtins.toJSON rice;
  };

  home = {
    sessionVariables = {
      EDITOR = "nvim";
      BROWSER =
        if pkgs.stdenv.isDarwin
        then "firefox"
        else "qutebrowser";
      LF_ICONS = "${builtins.readFile ./icons}";
      TERM = "xterm-256color";
      GO111MODULE = "on";
    };
    packages = with pkgs; [
      clang
      comma
      expect
      fd
      gh
      glow
      hactool
      htop
      imagemagick
      killall
      librespeed-cli
      luajit
      mpc_cli
      ncdu
      neofetch
      nix-diff
      nix-output-monitor
      nix-prefetch
      nixfmt
      nodejs
      pandoc
      radare2
      rclone
      rebuild
      ruby_3_0
      stylua
      tree
      unar
      update
      xplr
    ];
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

  programs = {
    home-manager = {
      enable = true;
      path = "...";
    };

    lazygit.enable = true;
    nix-index.enable = true;

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
        commands = {"Nix" = "${rebuild}";};
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
    bat = {
      enable = true;
      config = {theme = "base16";};
    };

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
        ''
        + builtins.readFile ./fish.fish;
    };
  };
}
