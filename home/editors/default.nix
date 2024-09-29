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
      checkmake
      editorconfig-checker

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
      eslint_d
      nodePackages.pnpm
      nodePackages.stylelint
      nodePackages.typescript
      nodePackages.typescript-language-server
      nodePackages.vscode-langservers-extracted
      nodejs
      prettierd
      yarn

      # nix
      alejandra
      manix
      nil
      statix

      # lua
      bandithedoge.fennel-language-server
      fennel
      fnlfmt
      luajit
      luajitPackages.luacheck
      selene
      stylua
      sumneko-lua-language-server

      # c
      bandithedoge.mesonlsp-bin
      clang
      clang-tools
      clazy
      cmake
      cmake-format
      gdb
      lldb
      neocmakelsp
      ninja
      pkg-config
      qt6.qtdeclarative
      qt6.qttools
      vscode-extensions.vadimcn.vscode-lldb.adapter

      # nim
      bandithedoge.nimlangserver

      # haskell
      cabal2nix
      ghc
      haskell-language-server
      haskellPackages.cabal-fmt
      haskellPackages.cabal-install
      haskellPackages.haskell-debug-adapter
      haskellPackages.hoogle
      haskellPackages.stack

      # yaml
      nodePackages.yaml-language-server
      actionlint

      # dart
      flutter

      # writing
      ltex-ls
      marksman
      typst

      # faust
      faust

      # zig
      zig
      zls

      # go
      go
      gopls

      # toml
      taplo
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
    extraLuaPackages = ps: with ps; [fennel];
    extraLuaConfig = with pkgs.rice; let
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
            nvim-web-devicons
            pathlib-nvim
            plenary-nvim
            popup-nvim
            sqlite-lua

            # dap
            nvim-dap
            nvim-dap-ui
            nvim-dap-virtual-text

            which-key-nvim

            # language-specific
            SchemaStore-nvim
            clangd_extensions-nvim
            dhall-vim
            faust-nvim
            flutter-tools-nvim
            haskell-tools-nvim
            lazydev-nvim
            nvim-luaref
            nvim-parinfer
            orgmode
            purescript-vim
            rasi-vim
            render-markdown-nvim
            telescope-manix
            tree-sitter-hypr
            vim-coffee-script
            vim-faust
            yuck-vim

            # lsp
            AstroLSP
            actions-preview-nvim
            conform-nvim
            fidget-nvim
            glance-nvim
            hover-nvim
            lsp_lines-nvim
            lsp_signature-nvim
            lsplinks-nvim
            neoconf-nvim
            nvim-lint
            nvim-lspconfig
            nvim-navic

            # ui
            diffview-nvim
            edgy-nvim
            FTerm-nvim
            gitsigns-nvim
            indent-blankline-nvim
            neogit
            nvim-highlight-colors
            nvim-hlslens
            nvim-notify
            tiny-devicons-auto-colors-nvim
            todo-comments-nvim
            trouble-nvim

            # utilities
            bigfile-nvim
            direnv-vim
            fold-cycle-nvim
            mkdir-nvim
            neogen
            nvim-expand-expr
            remember-nvim
            sort-nvim
            yanky-nvim

            # cmp
            cmp-cmdline
            cmp-git
            cmp-nvim-lsp
            cmp-path
            cmp-under-comparator
            cmp_luasnip
            friendly-snippets
            lspkind-nvim
            nvim-cmp
            nvim-snippets

            heirline-nvim

            mini-nvim

            neo-tree-nvim

            neorg

            # telescope
            dressing-nvim
            icon-picker-nvim
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
            nvim-ts-context-commentstring
            playground
            rainbow-delimiters-nvim
          ]));
    in ''
      ${def.lua}

      LAZY_PLUGINS = "${lazyPlugins}"
      USING_NIX = true

      vim.o.guifont = monoFont .. ":h16"

      require("tangerine").setup {
        target = vim.fn.stdpath "cache" .. "/tangerine",
        rtpdirs = {
          "after",
          "ftplugin"
        },
        compiler = {
          hooks = {"oninit"}
        }
      }

      require("config")
    '';
  };
  xdg.configFile."nvim" = {
    source = ./nvim;
    recursive = true;
    onChange = ''
      ${pkgs.lib.getExe config.programs.neovim.finalPackage} -E -c ":FnlCompile!" -c q
    '';
  };
  # }}}
}
