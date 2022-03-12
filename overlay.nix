final: prev: {
  dummy = prev.hello;

  wlroots = prev.wlroots.overrideAttrs (_: {
    version = "0.15"; # https://github.com/nix-community/nixpkgs-wayland/issues/313
  });

  vimPlugins = prev.vimExtraPlugins // prev.vimPlugins // {
    inherit (prev) parinfer-rust;
  };

  nim-unwrapped = prev.nim-unwrapped.overrideAttrs (_: {
    postInstall = ''
      ln -sf $out/nim/lib $out/lib
    '';
  });
}
