_: prev: {
  luaPackages =
    prev.luaPackages
    // {
      fennel = prev.luaPackages.buildLuaPackage {
        inherit (prev.fennel) pname version src buildInputs makeFlags;
        installPhase = ''
          mkdir -p $out
          cp fennel.lua $out/
        '';
      };
    };

  vimPlugins = prev.vimPlugins // prev.bandithedoge.vimPlugins;

  haskellPackages =
    prev.haskellPackages
    // {
      inherit (prev.bandithedoge) taffybar;
    };

  nim-unwrapped = prev.nim-unwrapped.overrideAttrs (oldAttrs: {
    installPhase =
      oldAttrs.installPhase
      + ''
        ln -sf $out/nim/lib $out/lib
      '';
  });
}
