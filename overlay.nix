_: prev: {
  rice = prev.callPackage ./rice.nix {};
  dummy = prev.hello;

  discord-canary = prev.discord-canary.override {withOpenASAR = true;};
}
