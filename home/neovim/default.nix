{ pkgs, inputs, ... }:
let rice = import ../../rice.nix;
in {

  home.packages = with pkgs; [
    # rust
    rust-analyzer
    rustfmt
    # python
    black
    codespell
    python39Packages.flake8
    python39Packages.isort
    python39Packages.python-lsp-server
    # shell
    nodePackages.bash-language-server
    shellcheck
    shfmt
    # web
    nodePackages.fixjson
    nodePackages.markdownlint-cli2
    nodePackages.prettier
    nodePackages.stylelint
    nodePackages.vscode-langservers-extracted
    yamllint
    # ruby
    rubocop
    solargraph
    # nix
    rnix-lsp
    statix
    # lua
    luajitPackages.luacheck
    # c
    cppcheck
  ];

  programs.neovim = {
    enable = true;
    package = pkgs.neovim-nightly;
    plugins = with pkgs.vimPlugins; [
      # treesitter
      (nvim-treesitter.withPlugins builtins.attrValues)
      nvim-autopairs
      nvim-ts-rainbow
      playground
      # libraries
      nui-nvim
      nvim-web-devicons
      plenary-nvim
      popup-nvim
      # utilities
      kommentary
      presence-nvim
      vim-automkdir
      # ui
      FTerm-nvim
      Shade-nvim
      blueballs-neovim
      bufferline-nvim
      gitsigns-nvim
      indent-blankline-nvim
      lualine-lsp-progress
      lualine-nvim
      nvim-colorizer-lua
      nvim-tree-lua
      telescope-dap-nvim
      telescope-fzy-native-nvim
      telescope-nvim
      telescope-symbols-nvim
      # keybindings
      which-key-nvim
      # lsp
      SchemaStore-nvim
      lsp_signature-nvim
      lspkind-nvim
      lspsaga-nvim
      null-ls-nvim
      nvim-lspconfig
      trouble-nvim
      # completion
      cmp-latex-symbols
      cmp-nvim-lsp
      cmp-nvim-lua
      cmp-path
      cmp-treesitter
      cmp_luasnip
      luasnip
      nvim-cmp
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
        require("config")
      EOF
    '';
  };
}
