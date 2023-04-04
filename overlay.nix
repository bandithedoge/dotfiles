final: prev: {
  rice = prev.callPackage ./rice.nix {};
  dummy = prev.hello;

  discord-canary = prev.discord-canary.override {withOpenASAR = true;};

  python310Packages =
    prev.python310Packages
    // {
      inherit (prev) poetry;
    };
}
