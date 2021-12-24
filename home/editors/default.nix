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
    # nim
    nimlsp
    # go
    gopls
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
      direnv-vim
      # ui
      FTerm-nvim
      Shade-nvim
      blueballs-neovim
      bufferline-nvim
      gitsigns-nvim
      neogit
      indent-blankline-nvim
      lualine-lsp-progress
      lualine-nvim
      nvim-colorizer-lua
      (chadtree.overrideAttrs (old: {
        buildInputs = [ pkgs.python3 ];
        buildPhase = ''
          python3 -m chadtree deps --nvim
        '';
      }))
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
      # language-specific
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

  programs.doom-emacs = {
    enable = true;
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
}
