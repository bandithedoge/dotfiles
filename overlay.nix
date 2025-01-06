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

  # https://github.com/NixOS/nixpkgs/issues/368651
  cnijfilter2 = prev.cnijfilter2.overrideAttrs (old: {
    patches =
      old.patches
      ++ [
        (prev.fetchpatch {
          url = "https://gitweb.gentoo.org/repo/gentoo.git/plain/net-print/cnijfilter2/files/cnijfilter2-6.60-c99.patch?id=d034f6ce2dd0ca63edd29156445f95d6d07dbbf4";
          hash = "sha256-9Voz7z1HrApQvo+Qsi0MZzkWcpBuxdTsZmpJXsO3o/E=";
        })
      ];
  });

  python3Packages =
    prev.python3Packages
    // {
      pyliblo = prev.python3Packages.pyliblo.overrideAttrs (_: {
        NIX_CFLAGS_COMPILE = ["-Wno-incompatible-pointer-types"];
      });
    };
}
