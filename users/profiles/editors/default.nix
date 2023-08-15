{
  pkgs,
  config,
  ...
}: {
  # common packages {{{
  home.packages = with pkgs; [
    act
    checkmake
    editorconfig-checker
    qtcreator

    # rust
    rust-analyzer
    rustfmt

    # python
    poetry
    black
    isort
    python311Packages.pyls-isort
    python311Packages.python-lsp-black
    python311Packages.python-lsp-server

    # shell
    nodePackages.bash-language-server
    shellcheck
    shellharden
    shfmt

    # web
    bandithedoge.nodePackages.emmet-language-server
    bandithedoge.nodePackages.tailwindcss-language-server
    nodePackages.eslint
    nodePackages.fixjson
    nodePackages.markdownlint-cli2
    nodePackages.pnpm
    nodePackages.prettier-plugin-toml
    nodePackages.stylelint
    nodePackages.typescript
    nodePackages.typescript-language-server
    nodePackages.vscode-langservers-extracted
    prettierd
    yarn

    # ruby
    rubocop
    solargraph

    # nix
    alejandra
    # nil
    nixd

    # lua
    fennel
    fnlfmt
    luajitPackages.luacheck
    stylua
    sumneko-lua-language-server

    # c
    clang
    clang-tools
    cmake
    neocmakelsp
    ninja
    pkg-config
    qt6.qtdeclarative

    # nim
    nimlsp

    # go
    go
    gopls

    # haskell
    cabal2nix
    ghc
    haskell-language-server
    haskellPackages.cabal-fmt
    haskellPackages.cabal-install
    haskellPackages.stack

    # yaml
    nodePackages.yaml-language-server
    actionlint

    # purescript
    nodePackages.purescript-language-server
    purescript
    spago

    # dhall
    dhall
    dhall-lsp-server

    # dart
    flutter

    # writing
    marksman

    # faust
    faust
  ];
  # }}}

  # neovim {{{
  programs.neovim = {
    enable = true;
    plugins = with pkgs.bandithedoge.vimPlugins; [
      hibiscus-nvim
      tangerine-nvim
      nightfox-nvim
      lazy-nvim
    ];
    extraConfig = "set runtimepath^=${./nvim}";
    extraLuaConfig = with pkgs.rice; let
      lazyPlugins =
        pkgs.linkFarm "lazy-plugins"
        (map (drv: {
            name = drv.pname or drv.name;
            path = drv;
          })
          (with pkgs.bandithedoge.vimPlugins; [
            mini-nvim

            # treesitter
            (pkgs.symlinkJoin {
              name = "nvim-treesitter";
              paths = with pkgs.vimPlugins.nvim-treesitter;
                [
                  withAllGrammars
                ]
                ++ map pkgs.neovimUtils.grammarToPlugin allGrammars;
            })
            nvim-ts-autotag
            nvim-ts-context-commentstring
            nvim-ts-rainbow
            playground

            # libraries
            nui-nvim
            nvim-web-devicons
            plenary-nvim
            popup-nvim
            sqlite-lua

            # utilities
            direnv-vim
            editorconfig-nvim
            fold-cycle-nvim
            icon-picker-nvim
            mkdir-nvim
            neogen
            nvim-expand-expr
            presence-nvim
            remember-nvim
            sort-nvim
            yanky-nvim

            # ui
            FTerm-nvim
            cybu-nvim
            dressing-nvim
            fm-nvim
            foldsigns-nvim
            gitsigns-nvim
            hover-nvim
            indent-blankline-nvim
            lualine-nvim
            neo-tree-nvim
            neodim
            nvim-colorizer-lua
            nvim-hlslens
            pretty-fold-nvim
            todo-comments-nvim
            trouble-nvim

            # keybindings
            which-key-nvim

            # lsp
            document-color-nvim
            fidget-nvim
            glance-nvim
            lsp_lines-nvim
            lsp_signature-nvim
            lspkind-nvim
            null-ls-nvim
            nvim-lspconfig
            SchemaStore-nvim

            # completion
            cmp-cmdline
            cmp-nvim-lsp
            cmp-nvim-lsp-document-symbol
            cmp-nvim-lua
            cmp-path
            cmp-under-comparator
            cmp_luasnip
            nvim-cmp

            # dap
            nvim-dap
            nvim-dap-ui

            # writing
            pkgs.vimPlugins.neorg

            # language-specific
            crates-nvim
            dhall-vim
            faust-nvim
            flutter-tools-nvim
            haskell-tools-nvim
            lua-dev-nvim
            nim-nvim
            nvim-luaref
            nvim-parinfer
            package-info-nvim
            purescript-vim
            rasi-vim
            typescript-nvim
            vim-coffee-script
            vim-faust
            yaml-companion-nvim
            yuck-vim

            # telescope
            telescope-nvim
            telescope-zf-native-nvim

            # snippets
            friendly-snippets
            LuaSnip
          ]));
    in ''
      ${def.lua}

      LAZY_PLUGINS = "${lazyPlugins}"
      USING_NIX = true

      vim.o.guifont = monoFont .. ":h16"

      require("tangerine").setup {
        target = vim.fn.stdpath "cache" .. "/tangerine",
        rtpdirs = {"ftplugin"},
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
      ${config.programs.neovim.finalPackage}/bin/nvim -E -c ":FnlCompile" -c q
    '';
  };
  # }}}
}
