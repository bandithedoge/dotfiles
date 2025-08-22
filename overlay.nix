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

  equibop = prev.equibop.override {
    withTTS = false;
    withMiddleClickScroll = true;
  };

  wine = prev.wine-tkg-ntsync;

  # XXX: https://github.com/robbert-vdh/yabridge/issues/382
  yabridge = prev.yabridge.overrideAttrs (_: {
    src = prev.fetchFromGitHub {
      owner = "robbert-vdh";
      repo = "yabridge";
      rev = "17a95fdf992cbb8be0194c32a15b0a6d41093635";
      hash = "sha256-FddTGIK5SAwqeS0qVWoV3RUbAg5XqpY+B60fFdDXWaQ=";
    };
  });
}
