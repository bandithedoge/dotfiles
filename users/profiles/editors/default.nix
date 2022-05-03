{
  pkgs,
  config,
  ...
}: let
  rice = import ../../../rice.nix;
in {
  # common packages {{{
  home.packages = with pkgs; [
    # rust
    rust-analyzer
    rustfmt
    # python
    black
    codespell
    python310Packages.isort
    python310Packages.python-lsp-server
    (python310.withPackages (p: with p; [debugpy]))
    # shell
    nodePackages.bash-language-server
    shellcheck
    shellharden
    shfmt
    # web
    nodePackages.fixjson
    nodePackages.markdownlint-cli2
    nodePackages.prettier
    nodePackages.vscode-langservers-extracted
    # ruby
    rubocop
    solargraph
    # nix
    alejandra
    rnix-lsp
    statix
    # lua
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
    go
    gopls
    # php
    phpPackages.psalm
    # haskell
    haskellPackages.cabal-fmt
    haskell-language-server
    # zig
    zig
    zls
  ];
  # }}}

  # neovim {{{
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-nightly;
    plugins = with pkgs.vimPlugins; [
      aniseed
      impatient-nvim
      mini-nvim
      # treesitter
      (nvim-treesitter.withPlugins (_: pkgs.tree-sitter.allGrammars))
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
      # utilities
      Comment-nvim
      conjure
      direnv-vim
      editorconfig-nvim
      mkdir-nvim
      nvim-autopairs
      nvim-expand-expr
      presence-nvim
      sort-nvim
      # ui
      cheatsheet-nvim
      dressing-nvim
      fidget-nvim
      fm-nvim
      FTerm-nvim
      gitsigns-nvim
      indent-blankline-nvim
      lualine-nvim
      neorg-telescope
      nightfox-nvim
      nvim-colorizer-lua
      nvim-hlslens
      nvim-tree-lua
      sqlite-lua
      pretty-fold-nvim
      specs-nvim
      telescope-dap-nvim
      telescope-frecency-nvim
      telescope-fzy-native-nvim
      telescope-nvim
      telescope-symbols-nvim
      # keybindings
      which-key-nvim
      # lsp
      lsp_signature-nvim
      lspkind-nvim
      null-ls-nvim
      nvim-lspconfig
      SchemaStore-nvim
      # completion
      cmp-cmdline
      cmp-latex-symbols
      cmp-nvim-lsp
      cmp-nvim-lsp-document-symbol
      cmp-nvim-lua
      cmp-path
      cmp-under-comparator
      cmp_luasnip
      friendly-snippets
      LuaSnip
      nvim-cmp
      # dap
      nvim-dap
      nvim-dap-ui
      # writing
      neorg
      orgmode
      # language-specific
      crates-nvim
      lua-dev-nvim
      nim-nvim
      rasi-vim
      vim-parinfer
    ];
    extraConfig = with rice; ''
      lua << EOF
        ${def.lua}

        vim.o.guifont = monoFont .. ":h16"

        vim.g["aniseed#env"] = {
          module = "config.init",
          compile = true
        }
      EOF
    '';
  };
  xdg.configFile."nvim" = {
    source = ./nvim/config;
    recursive = true;
    onChange = ''
      rm -rf ${config.xdg.configHome}/nvim/lua/config
    '';
  };
  # }}}
}
