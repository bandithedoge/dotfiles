{
  pkgs,
  config,
  ...
}: {
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
      bandithedoge.nodePackages.tailwindcss-language-server
      bun
      nodePackages.stylelint
      nodePackages.typescript-language-server
      nodePackages.vscode-langservers-extracted
      oxlint
      prettierd

      # nix
      nixd
      nixfmt-rfc-style
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
      bandithedoge.zlint-bin
      zig
      zls

      # toml
      taplo

      # java
      openjdk
      jdt-language-server
    ];
    # }}}
  };

  # neovim {{{
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      hibiscus-nvim
      tangerine-nvim
      nightfox-nvim
      lazy-nvim
    ];

    extraConfig = "set runtimepath^=${./nvim}";
    extraPackages = with pkgs; [imagemagick];
    extraLuaPackages = ps:
      with ps; [
        fennel
        lua-utils-nvim
        luautf8
        pathlib-nvim
        sqlite
      ];
    extraLuaConfig = let
      lazyPlugins =
        pkgs.linkFarm "lazy-plugins"
        (map (drv: {
            name = drv.pname or drv.name;
            path = drv;
          })
          (with pkgs.vimPlugins; [
            nui-nvim
            nvim-nio
            plenary-nvim
            popup-nvim
            sqlite-lua

            which-key-nvim

            # language-specific
            lazydev-nvim
            nvim-luaref
            nvim-parinfer
            SchemaStore-nvim
            typst-preview-nvim
            vim-just
            yuck-vim

            # lsp
            actions-preview-nvim
            conform-nvim
            fidget-nvim
            garbage-day-nvim
            glance-nvim
            lsplinks-nvim
            neoconf-nvim
            nvim-lint
            nvim-lspconfig
            nvim-navic
            outline-nvim

            # ui
            colorful-winsep-nvim
            diffview-nvim
            edgy-nvim
            gitsigns-nvim
            nvim-highlight-colors
            nvim-hlslens
            todo-comments-nvim
            trouble-nvim

            # utilities
            blink-pairs
            direnv-vim
            fold-cycle-nvim
            mkdir-nvim
            neogen
            persistence-nvim
            presence-nvim
            remember-nvim
            sort-nvim

            # blink
            blink-cmp
            colorful-menu-nvim
            friendly-snippets

            heirline-nvim

            mini-nvim

            # neo-tree
            neo-tree-nvim
            nvim-window-picker

            neorg
            neorg-interim-ls

            snacks-nvim

            # telescope
            dressing-nvim
            icon-picker-nvim
            telescope-all-recent-nvim
            telescope-nvim
            telescope-zf-native-nvim

            # treesitter
            (pkgs.symlinkJoin {
              name = "nvim-treesitter";
              paths =
                [pkgs.vimPlugins.nvim-treesitter.withAllGrammars]
                ++ map pkgs.neovimUtils.grammarToPlugin pkgs.vimPlugins.nvim-treesitter.allGrammars;
            })
            hmts-nvim
            nvim-ts-autotag
            playground
            rainbow-delimiters-nvim
            treesj
            ts-comments-nvim
          ]));
    in
      with pkgs.rice; ''
        ${def.lua}

        USING_NIX = true
        LAZY_PLUGINS = "${lazyPlugins}"

        vim.o.guifont = monoFont .. ":h16"

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
