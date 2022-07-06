{
  pkgs,
  config,
  ...
}: let
  rice = import ../../../rice.nix;
in {
  # common packages {{{
  home.packages = with pkgs; [
    editorconfig-checker

    # rust
    rust-analyzer
    rustfmt

    # python
    poetry
    python310Packages.black
    python310Packages.isort
    python310Packages.pyls-isort
    python310Packages.python-lsp-black
    python310Packages.python-lsp-server

    # shell
    nodePackages.bash-language-server
    shellcheck
    shellharden
    shfmt

    # web
    nodePackages.eslint
    nodePackages.fixjson
    nodePackages.markdownlint-cli2
    nodePackages.pnpm
    nodePackages.prettier
    nodePackages.prettier-plugin-toml
    nodePackages.stylelint
    nodePackages.vscode-langservers-extracted
    nodePackages.typescript
    nodePackages.typescript-language-server
    yarn

    # ruby
    rubocop
    solargraph

    # nix
    alejandra
    rnix-lsp
    statix

    # lua
    fennel
    fnlfmt
    luajitPackages.luacheck
    sumneko-lua-language-server

    # c
    clang-tools
    cppcheck

    # nim
    nim
    nimlsp

    # go
    gopls

    # php
    phpPackages.psalm

    # haskell
    cabal2nix
    ghc
    haskell-language-server
    haskellPackages.cabal-fmt
    haskellPackages.cabal-install
    haskellPackages.stack

    # yaml
    nodePackages.yaml-language-server
  ];
  # }}}

  # neovim {{{
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-unwrapped;
    plugins = with pkgs.vimPlugins; [
      hibiscus-nvim
      impatient-nvim
      mini-nvim
      tangerine-nvim

      # treesitter
      (nvim-treesitter.withPlugins (_: pkgs.tree-sitter.allGrammars))
      nvim-ts-autotag
      nvim-ts-context-commentstring
      nvim-ts-rainbow
      nvim-yati
      playground
      spellsitter-nvim

      # libraries
      nui-nvim
      nvim-web-devicons
      plenary-nvim
      popup-nvim
      sqlite-lua

      # utilities
      Comment-nvim
      direnv-vim
      editorconfig-nvim
      mkdir-nvim
      neogen
      nvim-autopairs
      nvim-expand-expr
      presence-nvim
      remember-nvim
      sort-nvim

      # ui
      bufferline-nvim
      cinnamon-nvim
      dressing-nvim
      fidget-nvim
      flare-nvim
      fm-nvim
      foldsigns-nvim
      FTerm-nvim
      gitsigns-nvim
      glow-hover-nvim
      heirline-nvim
      indent-blankline-nvim
      lsp_lines-nvim
      lualine-nvim
      neorg-telescope
      numbers-nvim
      nvim-colorizer-lua
      nvim-hlslens
      nvim-tree-lua
      pretty-fold-nvim
      satellite-nvim
      todo-comments-nvim
      trouble-nvim

      # keybindings
      fold-cycle-nvim
      move-nvim
      which-key-nvim

      # lsp
      SchemaStore-nvim
      lsp_extensions-nvim
      lsp_signature-nvim
      lspkind-nvim
      null-ls-nvim
      nvim-lspconfig

      # completion
      cmp-cmdline
      cmp-latex-symbols
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
      neorg

      # language-specific
      crates-nvim
      lua-dev-nvim
      nim-vim
      nvim-luaref
      parinfer-rust
      rasi-vim
      yaml-companion-nvim
      vim-coffee-script

      # colors
      nightfox-nvim

      # telescope
      cheatsheet-nvim
      telescope-dap-nvim
      telescope-frecency-nvim
      telescope-fzy-native-nvim
      telescope-nvim
      telescope-symbols-nvim

      # snippets
      friendly-snippets
      LuaSnip
    ];
    extraConfig = with rice; ''
      set runtimepath^=${./nvim}

      lua << EOF
        ${def.lua}

        vim.o.guifont = monoFont .. ":h16"

        require "impatient"

        require("tangerine").setup {
          target = vim.fn.stdpath [[cache]] .. "/tangerine",
          rtpdirs = {"ftplugin"},
          compiler = {
            hooks = {"oninit"}
          }
        }
      EOF
    '';
  };
  xdg.configFile."nvim" = {
    source = ./nvim;
    recursive = true;
    onChange = ''
      rm -rf ${config.xdg.cacheHome}/nvim/tangerine
      nvim -E -c ":FnlCompile" -c q
    '';
  };
  # }}}
}
