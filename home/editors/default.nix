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
      rustfmt

      # python
      basedpyright
      poetry
      python3
      python3Packages.debugpy
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
      alejandra
      nixd
      statix

      # lua
      bandithedoge.fennel-language-server
      fennel
      fnlfmt
      luajit
      stylua
      sumneko-lua-language-server

      # c
      bandithedoge.mesonlsp-bin
      clang-tools
      cmake-lint
      lldb
      neocmakelsp

      # nim
      bandithedoge.nimlangserver

      # haskell
      cabal2nix
      haskell-language-server
      haskellPackages.cabal-fmt

      # yaml
      nodePackages.yaml-language-server
      actionlint

      # writing
      ltex-ls
      marksman
      tinymist
      typst
      typstyle

      # zig
      bandithedoge.zlint-bin
      zig
      zls

      # go
      gopls

      # toml
      taplo

      julia

      blueprint-compiler

      slint-lsp
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
        luautf8
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
            astrocore
            lua-utils-nvim
            nui-nvim
            nvim-nio
            pathlib-nvim
            plenary-nvim
            popup-nvim
            sqlite-lua

            which-key-nvim

            # language-specific
            clangd_extensions-nvim
            dhall-vim
            faust-nvim
            flutter-tools-nvim
            haskell-tools-nvim
            lazydev-nvim
            ltex_extra-nvim
            nvim-luaref
            nvim-parinfer
            orgmode
            purescript-vim
            rasi-vim
            SchemaStore-nvim
            tree-sitter-hypr
            typst-preview-nvim
            vim-coffee-script
            vim-faust
            vim-just
            vim-slint
            yuck-vim

            # lsp
            actions-preview-nvim
            AstroLSP
            conform-nvim
            fidget-nvim
            garbage-day-nvim
            glance-nvim
            hover-nvim
            lsp_lines-nvim
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
            neogit
            nvim-highlight-colors
            nvim-hlslens
            todo-comments-nvim
            trouble-nvim

            # utilities
            direnv-vim
            fold-cycle-nvim
            mkdir-nvim
            neogen
            nix-develop-nvim
            nvim-expand-expr
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
                ++ map pkgs.neovimUtils.grammarToPlugin (pkgs.vimPlugins.nvim-treesitter.allGrammars
                  ++ (with pkgs.bandithedoge.treeSitterGrammars; [tree-sitter-hypr]));
            })
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
