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

  # https://github.com/robbert-vdh/yabridge/issues/382
  yabridge = prev.yabridge.overrideAttrs (_: {
    src = prev.fetchFromGitHub {
      owner = "robbert-vdh";
      repo = "yabridge";
      rev = "new-wine10-embedding";
      hash = "sha256-vgG6hwB/1f4qHGRQBnXMSvafTzQKT8GJeMhcco/M8JQ=";
    };
  });

  # https://github.com/NixOS/nixpkgs/pull/406514
  cozette = prev.cozette.overrideAttrs (_: {
    postInstall = ''
      install -Dm644 *.psf -t $out/share/consolefonts
    '';
  });

  # https://nixpk.gs/pr-tracker.html?pr=411777
  linuxPackages_cachyos = prev.linuxPackages_cachyos.extend (_: prev': {
    v4l2loopback = prev'.v4l2loopback.overrideAttrs (_: rec {
      version = "0.15.0";
      src = final.fetchFromGitHub {
        owner = "umlaeute";
        repo = "v4l2loopback";
        rev = "v${version}";
        hash = "sha256-fa3f8GDoQTkPppAysrkA7kHuU5z2P2pqI8dKhuKYh04=";
      };
    });
  });
}
