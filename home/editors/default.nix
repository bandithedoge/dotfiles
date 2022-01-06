{ pkgs, ... }@inputs:
let rice = import ../../rice.nix;
in {
  # lsp/linters/formatters {{{
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
      comment-nvim
      direnv-vim
      presence-nvim
      vim-automkdir
      # ui
      FTerm-nvim
      Shade-nvim
      blueballs-neovim
      gitsigns-nvim
      indent-blankline-nvim
      lualine-lsp-progress
      lualine-nvim
      neogit
      nvim-colorizer-lua
      nvim-gps
      telescope-dap-nvim
      telescope-fzy-native-nvim
      telescope-nvim
      telescope-symbols-nvim
      (chadtree.overrideAttrs (old: {
        buildInputs = [ pkgs.python3 ];
        buildPhase = ''
          python3 -m chadtree deps --nvim
        '';
      }))
      # keybindings
      which-key-nvim
      # lsp
      SchemaStore-nvim
      lsp_signature-nvim
      lspkind-nvim
      lspsaga-nvim
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
      nim-vim
    ];
    extraConfig = ''
      set runtimepath ^=${./.}
      lua << EOF
        vim.o.guifont = "${rice.monoFont}:h16"

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
