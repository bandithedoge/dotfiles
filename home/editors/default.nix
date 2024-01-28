{
  pkgs,
  config,
  ...
}:
{
  imports = [./emacs.nix];

  # common packages {{{
  home.packages = with pkgs; [
    act
    checkmake
    editorconfig-checker

    # rust
    rust-analyzer
    rustfmt

    # python
    poetry
    python3
    python3Packages.debugpy
    python3Packages.python-lsp-server
    ruff-lsp

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
    nodePackages.prettier
    nodePackages.prettier-plugin-toml
    nodePackages.stylelint
    nodePackages.typescript
    nodePackages.typescript-language-server
    nodePackages.vscode-langservers-extracted
    yarn

    # nix
    alejandra
    nil

    # lua
    fennel
    fnlfmt
    luajit
    luajitPackages.luacheck
    selene
    stylua
    sumneko-lua-language-server

    # c
    bandithedoge.swift-mesonlsp-bin
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

    # nim
    bandithedoge.nimlangserver

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

    # dart
    flutter

    # writing
    ltex-ls
    marksman
    python3Packages.grip

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
              paths =
                [pkgs.vimPlugins.nvim-treesitter.withAllGrammars]
                ++ map pkgs.neovimUtils.grammarToPlugin (pkgs.vimPlugins.nvim-treesitter.allGrammars
                  ++ (with pkgs.bandithedoge.treeSitterGrammars; [tree-sitter-hypr]));
            })
            nvim-ts-autotag
            nvim-ts-context-commentstring
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
            remember-nvim
            sort-nvim
            yanky-nvim

            # ui
            bufferline-nvim
            cybu-nvim
            dressing-nvim
            fm-nvim
            foldsigns-nvim
            FTerm-nvim
            gitsigns-nvim
            heirline-nvim
            hover-nvim
            indent-blankline-nvim
            nvim-colorizer-lua
            nvim-hlslens
            nvim-tree-lua
            pretty-fold-nvim
            rainbow-delimiters-nvim
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
            lspsaga-nvim
            none-ls-nvim
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
            neodev-nvim
            nvim-luaref
            nvim-parinfer
            package-info-nvim
            purescript-vim
            rasi-vim
            tree-sitter-hypr
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
}
