final: prev: {
  rice = prev.callPackage ./rice.nix {};
  dummy = prev.hello;

  discord = prev.discord.override {
    withOpenASAR = true;
    withVencord = true;
  };

}
