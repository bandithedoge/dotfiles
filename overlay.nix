_: prev: {
  # wlroots = prev.wlroots.overrideAttrs (_: {
  #   version = "0.15";
  # });

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
}
