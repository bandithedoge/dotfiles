{ pkgs, inputs, ... }: {

  home.packages = with pkgs; [
    rust-analyzer
    python39Packages.python-lsp-server
    nodePackages.bash-language-server
    nodePackages.vscode-langservers-extracted
    clang-tools
    rubyPackages_3_0.solargraph
    haskell-language-server
    rnix-lsp
  ];

  programs.neovim = {
    enable = true;
    package = pkgs.neovim-nightly;
    plugins = with pkgs.vimPlugins; [
      # treesitter
      (nvim-treesitter.withPlugins (p: builtins.attrValues p))
      nvim-ts-rainbow
      playground
      nvim-autopairs
      # libraries
      nvim-web-devicons
      plenary-nvim
      popup-nvim
      # utilities
      kommentary
      formatter-nvim
      vim-automkdir
      presence-nvim
      # ui
      blueballs-neovim
      lualine-nvim
      bufferline-nvim
      indent-blankline-nvim
      nvim-colorizer-lua
      gitsigns-nvim
      nvim-tree-lua
      telescope-nvim
      telescope-symbols-nvim
      lazygit-nvim
      /* FTerm-nvim */
      # keybindings
      which-key-nvim
      # lsp
      nvim-lspconfig
      trouble-nvim
      lspsaga-nvim
      lspkind-nvim
      nvim-lightbulb
      SchemaStore-nvim
      /* nvim-lint */
      # completion
      luasnip
      nvim-cmp
      cmp-nvim-lsp
      cmp-path
      cmp_luasnip
      cmp-treesitter
      cmp-nvim-lua
      cmp-latex-symbols
      # dap
      nvim-dap
      nvim-dap-ui
      # writing
      neorg
      orgmode-nvim
    ];
    extraConfig = ''
      set runtimepath ^=${../neovim}
      lua << EOF
        require("config.init")
      EOF
    '';
  };
}
