{
  pkgs,
  config,
  ...
}: let
  rice = import ../../../rice.nix {inherit pkgs;};
in {
  # common packages {{{
  home.packages = with pkgs; [
    editorconfig-checker

    # rust
    rust-analyzer
    rustfmt

    # python
    python310Packages.poetry
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
    bandithedoge.nodePackages.emmet-ls
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

    # purescript
    nodePackages.purescript-language-server
    purescript
    spago

    # dhall
    dhall
    dhall-lsp-server
  ];
  # }}}

  # neovim {{{
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-unwrapped;
    plugins = with pkgs.vimPlugins;
    with pkgs.bandithedoge.vimPlugins; [
      hibiscus-nvim
      impatient-nvim
      mini-nvim
      tangerine-nvim

      # treesitter
      (nvim-treesitter.withPlugins (_: (builtins.filter pkgs.lib.isDerivation (builtins.attrValues pkgs.tree-sitter-grammars))))
      nvim-ts-autotag
      nvim-ts-context-commentstring
      nvim-ts-rainbow
      playground
      spellsitter-nvim

      # libraries
      nui-nvim
      nvim-web-devicons
      plenary-nvim
      popup-nvim
      sqlite-lua

      # utilities
      colortils-nvim
      Comment-nvim
      detect-language-nvim
      direnv-vim
      editorconfig-nvim
      mkdir-nvim
      neogen
      nvim-autopairs
      nvim-expand-expr
      presence-nvim
      remember-nvim
      sort-nvim
      stabilize-nvim

      # ui
      barbar-nvim
      cinnamon-nvim
      dressing-nvim
      fidget-nvim
      flare-nvim
      fm-nvim
      foldsigns-nvim
      FTerm-nvim
      galaxyline-nvim
      gitsigns-nvim
      glow-hover-nvim
      heirline-nvim
      hover-nvim
      incline-nvim
      indent-blankline-nvim
      lsp_lines-nvim
      neo-tree-nvim
      neodim
      neorg-telescope
      numbers-nvim
      nvim-colorizer-lua
      nvim-hlslens
      nvim-notify
      pretty-fold-nvim
      todo-comments-nvim
      trouble-nvim

      # keybindings
      fold-cycle-nvim
      icon-picker-nvim
      which-key-nvim

      # lsp
      document-color-nvim
      lsp_extensions-nvim
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
      neorg

      # language-specific
      crates-nvim
      dhall-vim
      lua-dev-nvim
      nim-nvim
      nvim-luaref
      nvim-parinfer
      package-info-nvim
      purescript-vim
      rasi-vim
      vim-coffee-script
      yaml-companion-nvim
      yuck-vim

      # colors
      nightfox-nvim

      # telescope
      telescope-dap-nvim
      telescope-frecency-nvim
      telescope-nvim
      telescope-zf-native-nvim

      # snippets
      friendly-snippets
      LuaSnip
    ];
    extraConfig = with rice; ''
      set runtimepath^=${./nvim}

      lua << EOF
        ${def.lua}

        vim.o.guifont = monoFont .. ":h16"

        require("impatient").enable_profile()

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
