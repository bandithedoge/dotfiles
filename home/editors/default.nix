{
  pkgs,
  config,
  ...
}:
{
  # imports = [./emacs.nix];

  home = {
    sessionVariables.EDITOR = "nvim";
    packages = with pkgs; [
      # {{{
      # rust
      rust-analyzer

      # python
      basedpyright
      poetry
      python3
      ruff

      # shell
      nodePackages.bash-language-server
      shellcheck
      shfmt

      # web
      bandithedoge.nodePackages.emmet-language-server
      biome
      bun
      nodePackages.vscode-langservers-extracted
      oxlint
      prettierd
      stylelint-lsp
      superhtml

      # nix
      nixd
      nixfmt-rfc-style
      nixpkgs-hammering
      statix

      # lua
      bandithedoge.fennel-language-server
      fennel
      fnlfmt
      luajit
      stylua
      sumneko-lua-language-server

      # c
      clang-tools
      lldb
      mesonlsp
      neocmakelsp

      # yaml
      nodePackages.yaml-language-server

      # writing
      harper
      marksman
      tinymist
      typst
      typstyle

      # zig
      zig
      zig-zlint
      zls

      # toml
      taplo
    ];
    # }}}
  };

  # neovim {{{
  programs.neovim = {
    enable = true;
    plugins =
      with pkgs.vimPlugins;
      [
        hibiscus-nvim
        tangerine-nvim
        nightfox-nvim

        nvim-parinfer
        yuck-vim

        friendly-snippets

        mini-nvim

        snacks-nvim
      ]
      ++ (builtins.map
        (x: {
          plugin = x;
          optional = true;
        })
        [
          which-key-nvim

          lazydev-nvim
          typst-preview-nvim

          SchemaStore-nvim
          actions-preview-nvim
          conform-nvim
          fidget-nvim
          glance-nvim
          lsplinks-nvim
          neoconf-nvim
          nvim-lint
          nvim-lspconfig
          nvim-navic
          outline-nvim

          edgy-nvim
          gitsigns-nvim
          nvim-highlight-colors
          nvim-hlslens
          todo-comments-nvim
          trouble-nvim

          blink-pairs
          direnv-vim
          fold-cycle-nvim
          mkdir-nvim
          neogen
          persistence-nvim
          presence-nvim
          remember-nvim
          sort-nvim

          blink-cmp
          colorful-menu-nvim

          heirline-nvim

          neo-tree-nvim
          nvim-window-picker

          neorg
          neorg-interim-ls

          hmts-nvim
          nvim-treesitter.withAllGrammars
          nvim-ts-autotag
          playground
          rainbow-delimiters-nvim
          treesj
          ts-comments-nvim
        ]
      );
    extraPackages = with pkgs; [ imagemagick ];
    extraLuaPackages =
      ps: with ps; [
        fennel
        lua-utils-nvim
        luautf8
        lz-n
        nui-nvim
        nvim-nio
        pathlib-nvim
        plenary-nvim
      ];
    extraLuaConfig =
      with pkgs.rice;
      # lua
      ''
        ${def.lua}

        vim.o.guifont = monoFont .. ":h16"

        vim.opt.runtimepath:append("${./nvim}")

        require("tangerine").setup {
          target = vim.fn.stdpath "cache" .. "/tangerine",
          rtpdirs = {
            "after",
            "ftplugin"
          },
          compiler = {
            hooks = {"oninit"}
          },
          keymaps = {
            eval_buffer = "<Nop>",
            peek_buffer = "<Nop>",
            goto_output = "<Nop>"
          }
        }

        require("config")
      '';
  };
  xdg.configFile = {
    "nvim" = {
      source = ./nvim;
      recursive = true;
      onChange = ''
        ${pkgs.lib.getExe config.programs.neovim.finalPackage} -E -c ":FnlCompile!" -c q
      '';
    };
  };
  # }}}
}
