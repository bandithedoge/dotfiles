final: prev: {
  rice = prev.callPackage ./rice.nix {};
  dummy = prev.hello;

  vimPlugins =
    prev.vimPlugins
    // prev.bandithedoge.vimPlugins
    // prev.awesomeNeovimPlugins
    // {inherit (prev.vimPlugins) neorg nvim-treesitter;};

  discord = prev.discord.override {
    withOpenASAR = true;
    withVencord = true;
  };
}
