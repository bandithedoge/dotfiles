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
    sumneko-lua-language-server
    # c
    cppcheck
    # nim
    nimlsp
    nim
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
      nvim-expand-expr
      presence-nvim
      sort-nvim
      vim-automkdir
      # ui
      fm-nvim
      FTerm-nvim
      gitsigns-nvim
      indent-blankline-nvim
      lualine-lsp-progress
      lualine-nvim
      neogit
      nightfox-nvim
      nvim-colorizer-lua
      nvim-gps
      nvim-hlslens
      nvim-tree-lua
      pretty-fold-nvim
      specs-nvim
      telescope-dap-nvim
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

  # doom emacs {{{
  programs.doom-emacs = {
    enable = false;
    emacsPackage = if pkgs.stdenv.isDarwin then
      pkgs.emacsMacport.overrideAttrs (oldAttrs: {
        configureFlags = oldAttrs.configureFlags
          ++ [ "--with-native-compilation" ];
      })
    else
      (pkgs.emacsUnstable.override {
        nativeComp = true;
        withSQLite3 = true;
      }).overrideAttrs (oldAttrs: {
        configureFlags = (pkgs.lib.remove "--with-xft" oldAttrs.configureFlags)
          ++ [ "--with-pgtk" ];
      });
    emacsPackagesOverlay = self: super:
      with pkgs.emacsPackages; {
        gitignore-mode = git-modes;
        gitconfig-mode = git-modes;
      };
    doomPrivateDir = ./doom.d;
    extraConfig = ''
      (setq doom-font (font-spec :family "${rice.monoFont}" :size 14))
      ${
        if !pkgs.stdenv.isDarwin then
          ''
            (setq doom-variable-pitch-font (font-spec :family "${rice.uiFont}" :size 14))''
        else
          ""
      })
    '';
  };
  # }}}

  # vscode {{{
  programs.vscode = {
    enable = false;
    package = pkgs.vscodium;
    extensions = with pkgs.vscode-extensions; [
      coenraads.bracket-pair-colorizer
      eamodio.gitlens
      mvllow.rose-pine
    ];
  };
  # }}}
}
