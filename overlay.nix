final: prev: {
  dummy = prev.hello;

  wlroots = (prev.wlroots.overrideAttrs (_: {
    version = "0.15"; # https://github.com/nix-community/nixpkgs-wayland/issues/313
  })).override { enableXWayland = false; };

  vimPlugins = prev.vimExtraPlugins // prev.vimPlugins;
}
