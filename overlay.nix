final: prev: {
  rice = prev.callPackage ./rice.nix { };
  dummy = prev.hello;

  vimPlugins =
    let
      vP = builtins.mapAttrs (
        _: p:
        p.overrideAttrs (old: {
          pname = builtins.replaceStrings [ "." ] [ "-" ] old.pname;
        })
      ) prev.vimPlugins;
    in
    vP
    // prev.bandithedoge.vimPlugins
    // prev.awesomeNeovimPlugins
    // {
      inherit (vP) nvim-treesitter blink-cmp;

      blink-pairs = vP.blink-pairs.overrideAttrs (_: {
        doCheck = false;
      });
    };

  yaziPlugins = prev.yaziPlugins // prev.bandithedoge.yaziPlugins;

  discord = prev.discord.override {
    withOpenASAR = true;
    withVencord = true;
  };

  connman-gtk = prev.connman-gtk.overrideAttrs (_: {
    NIX_CFLAGS_COMPILE = [ "-Wno-incompatible-pointer-types" ];
  });

  # gamescope = prev.gamescope_git.overrideAttrs (_: {
  #   NIX_CFLAGS_COMPILE = ["-fno-fast-math"];
  #   # inherit (prev.gamescope) postPatch;
  # });

  equibop = prev.equibop.override {
    withTTS = false;
    withMiddleClickScroll = true;
  };

  furnace = prev.furnace.override {
    withGL = false;
  };

  wine = prev.wine-tkg;

  # HACK: https://github.com/robbert-vdh/yabridge/issues/382
  yabridge = prev.yabridge.overrideAttrs (_: {
    src = prev.fetchFromGitHub {
      owner = "robbert-vdh";
      repo = "yabridge";
      rev = "17a95fdf992cbb8be0194c32a15b0a6d41093635";
      hash = "sha256-FddTGIK5SAwqeS0qVWoV3RUbAg5XqpY+B60fFdDXWaQ=";
    };
  });

  # HACK: https://github.com/NixOS/nixpkgs/issues/421442
  ghostty = prev.ghostty.overrideAttrs (_: {
    preBuild = ''
      shopt -s globstar
      sed -i 's/^const xev = @import("xev");$/const xev = @import("xev").Epoll;/' **/*.zig
      shopt -u globstar
    '';
  });
}
