{ pkgs, inputs, ... }:
let rice = import ../../rice.nix;
in {

  home.packages = with pkgs; [
    rust-analyzer
    python39Packages.python-lsp-server
    nodePackages.bash-language-server
    nodePackages.vscode-langservers-extracted
    rubyPackages_3_0.solargraph
    rnix-lsp
    luajitPackages.luacheck
    html-tidy
    languagetool
    nodePackages.markdownlint-cli2
    python39Packages.pylint
    shellcheck
    statix
    nodePackages.prettier
  ];

  programs.neovim = {
    enable = true;
    package = pkgs.neovim-nightly;
    plugins = with pkgs.vimPlugins; [
      # treesitter
      (nvim-treesitter.withPlugins (plugins:
        with plugins; [
          tree-sitter-nix
          tree-sitter-lua
          tree-sitter-norg
          tree-sitter-cpp
          tree-sitter-python
        ]))
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
      telescope-dap-nvim
      telescope-fzy-native-nvim
      FTerm-nvim
      Shade-nvim
      # keybindings
      which-key-nvim
      # lsp
      nvim-lspconfig
      trouble-nvim
      lspsaga-nvim
      lspkind-nvim
      SchemaStore-nvim
      nvim-lint
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
      orgmode
    ];
    extraConfig = ''
      set runtimepath ^=${./.}
      lua << EOF
        vim.o.guifont = "${rice.monoFont}:h16"
        require("config.init")
      EOF
    '';
  };
}
