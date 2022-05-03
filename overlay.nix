final: prev: {
  # wlroots = prev.wlroots.overrideAttrs (oldAttrs: {
  #   buildInputs = oldAttrs.buildInputs ++ [ prev.vulkan-headers ];
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
