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
    clang-tools
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
      fm-nvim
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
      nim-nvim
      rasi-vim
    ];
    extraConfig = with rice; ''
      set runtimepath ^=${./.}
      lua << EOF
        vim.o.guifont = "${monoFont}:h16"

        base00 = "${base00}"
        base01 = "${base01}"
        base02 = "${base02}"
        base03 = "${base03}"
        base04 = "${base04}"
        base05 = "${base05}"
        base06 = "${base06}"
        base07 = "${base07}"
        base08 = "${base08}"
        base09 = "${base09}"
        base0A = "${base0A}"
        base0B = "${base0B}"
        base0C = "${base0C}"
        base0D = "${base0D}"
        base0E = "${base0E}"
        base0F = "${base0F}"

        base10 = "${base10}"
        base11 = "${base11}"
        base12 = "${base12}"
        base13 = "${base13}"
        base14 = "${base14}"
        base15 = "${base15}"
        base16 = "${base16}"
        base17 = "${base17}"

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
