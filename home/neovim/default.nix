{ pkgs, inputs, ... }:
let rice = import ../../rice.nix;
in {

  home.packages = with pkgs; [
    # rust
    rust-analyzer
    rustfmt
    # python
    python39Packages.python-lsp-server
    python39Packages.isort
    python39Packages.flake8
    black
    codespell
    # shell
    nodePackages.bash-language-server
    shellcheck
    shfmt
    # web
    nodePackages.vscode-langservers-extracted
    nodePackages.fixjson
    nodePackages.stylelint
    yamllint
    nodePackages.prettier
    nodePackages.markdownlint-cli2
    # ruby
    solargraph
    rubocop
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
      (nvim-treesitter.withPlugins (p: builtins.attrValues p))
      nvim-ts-rainbow
      playground
      nvim-autopairs
      # libraries
      nvim-web-devicons
      plenary-nvim
      popup-nvim
      nui-nvim
      # utilities
      kommentary
      vim-automkdir
      presence-nvim
      # ui
      blueballs-neovim
      lualine-nvim
      lualine-lsp-progress
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
      lsp_signature-nvim
      lspsaga-nvim
      lspkind-nvim
      SchemaStore-nvim
      null-ls-nvim
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
        require("config")
      EOF
    '';
  };
}
