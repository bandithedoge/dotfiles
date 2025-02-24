final: prev: {
  rice = prev.callPackage ./rice.nix {};
  dummy = prev.hello;

  vimPlugins = let
    vP = builtins.mapAttrs (_: p: p.overrideAttrs (old: {pname = builtins.replaceStrings ["."] ["-"] old.pname;})) prev.vimPlugins;
  in
    vP
    // prev.bandithedoge.vimPlugins
    // prev.awesomeNeovimPlugins
    // {inherit (vP) neorg nvim-treesitter blink-cmp;};

  discord = prev.discord.override {
    withOpenASAR = true;
    withVencord = true;
  };

  connman-gtk = prev.connman-gtk.overrideAttrs (_: {
    NIX_CFLAGS_COMPILE = ["-Wno-incompatible-pointer-types"];
  });

  gamescope = prev.gamescope_git.overrideAttrs (_: {
    NIX_CFLAGS_COMPILE = ["-fno-fast-math"];
    inherit (prev.gamescope) postPatch;
  });
}
