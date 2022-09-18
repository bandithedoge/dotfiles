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

  discord-canary = prev.discord-canary.override {withOpenASAR = true;};
}
