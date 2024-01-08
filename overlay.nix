final: prev: {
  rice = prev.callPackage ./rice.nix {};
  dummy = prev.hello;

  discord = prev.discord.override {withOpenASAR = true;};

  # python310Packages =
  #   prev.python310Packages
  #   // {
  #     inherit (prev) poetry;
  #   };
}
