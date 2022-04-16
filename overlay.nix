final: prev: {
  dummy = prev.hello;

  vimPlugins = prev.vimExtraPlugins // prev.vimPlugins // {
    inherit (prev) parinfer-rust;
  };

  luaPackages = prev.luaPackages // {
    fennel = prev.luaPackages.buildLuaPackage {
      inherit (prev.fennel) pname version src buildInputs makeFlags;
      installPhase = ''
        mkdir -p $out
        cp fennel.lua $out/
      '';
    };
  };
}
