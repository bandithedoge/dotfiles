final: prev: {
  rice = prev.callPackage ./rice.nix {};
  dummy = prev.hello;

  vimPlugins =
    (builtins.mapAttrs (_: p: p.overrideAttrs (old: {pname = builtins.replaceStrings ["."] ["-"] old.pname;})) prev.vimPlugins)
    // prev.bandithedoge.vimPlugins
    // prev.awesomeNeovimPlugins
    // {inherit (prev.vimPlugins) neorg nvim-treesitter;};

  discord = prev.discord.override {
    withOpenASAR = true;
    withVencord = true;
  };

  connman-gtk = prev.connman-gtk.overrideAttrs (_: {
    NIX_CFLAGS_COMPILE = ["-Wno-incompatible-pointer-types"];
  });
}
