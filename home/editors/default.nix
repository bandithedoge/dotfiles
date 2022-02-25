{ pkgs, ... }@inputs:
let rice = import ../../rice.nix;
in
{
  # common packages {{{
  home.packages = with pkgs; [
    # rust
    rust-analyzer
    rustfmt
    # python
    black
    codespell
    python39Packages.isort
    python39Packages.python-lsp-server
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
    rnix-lsp
    statix
    # lua
    luajitPackages.luacheck
    sumneko-lua-language-server
    # c
    clang-tools
    cppcheck
    # nim
    nim-unwrapped
    # go
    gopls
  ];
  # }}}

  # neovim {{{
  programs.neovim = {
    enable = true;
    package = pkgs.neovim-nightly;
    plugins = with pkgs.vimPlugins; [
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
      direnv-vim
      editorconfig-nvim
      nvim-expand-expr
      presence-nvim
      sort-nvim
      vim-automkdir
      # ui
      cheatsheet-nvim
      dressing-nvim
      fidget-nvim
      fm-nvim
      FTerm-nvim
      gitsigns-nvim
      indent-blankline-nvim
      lsp_lines-nvim
      lualine-lsp-progress
      lualine-nvim
      neogit
      neorg-telescope
      nightfox-nvim
      nvim-cokeline
      nvim-colorizer-lua
      nvim-gps
      nvim-hlslens
      nvim-tree-lua
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
      cmp-emoji
      cmp-latex-symbols
      cmp-nvim-lsp
      cmp-nvim-lsp-document-symbol
      cmp-nvim-lua
      cmp-path
      cmp-treesitter
      cmp-under-comparator
      cmp_luasnip
      friendly-snippets
      luasnip
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
    ];
    extraConfig = with rice; ''
      set runtimepath ^=${./.}
      lua << EOF
        ${def.lua}

        vim.o.guifont = monoFont .. ":h16"

        require("config")
      EOF
    '';
  };
  # }}}
}
