{ pkgs, ... }@inputs:
let rice = import ../../rice.nix;
in {
  # common packages {{{
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
    # nim
    nimlsp
    # go
    gopls
  ];
  # }}}

  # neovim {{{
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimExtraPlugins // pkgs.vimPlugins; [
      impatient-nvim
      mini-nvim
      # treesitter
      (nvim-treesitter.withPlugins builtins.attrValues)
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
      direnv-vim
      nvim-expand-expr
      presence-nvim
      vim-automkdir
      # ui
      FTerm-nvim
      Shade-nvim
      blueballs-neovim
      fm-nvim
      gitsigns-nvim
      indent-blankline-nvim
      lualine-lsp-progress
      lualine-nvim
      neogit
      nvim-colorizer-lua
      nvim-gps
      nvim-hlslens
      nvim-scrollbar
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
      null-ls-nvim
      nvim-lspconfig
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
      nim-vim
      rasi-vim
    ];
    extraConfig = with rice; ''
      set runtimepath ^=${./.}
      lua << EOF
        require("impatient").enable_profile()
        vim.o.guifont = "${monoFont}:h16"
        require("config")
      EOF
    '';
  };
  # }}}

  # doom emacs {{{
  programs.doom-emacs = {
    # enable = true;
    emacsPackage =
      if pkgs.stdenv.isDarwin then pkgs.emacsMacport else pkgs.emacs;
    emacsPackagesOverlay = self: super:
      with pkgs.emacsPackages; {
        gitignore-mode = git-modes;
        gitconfig-mode = git-modes;
      };
    doomPrivateDir = ./doom.d;
    extraConfig = ''
      (setq doom-font (font-spec :family "${rice.monoFont}" :size 14))
    '';
  };
  # }}}

  # vscode {{{
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions; [
      coenraads.bracket-pair-colorizer
      eamodio.gitlens
      mvllow.rose-pine
    ];
  };
  # }}}
}
