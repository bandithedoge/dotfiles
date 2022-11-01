_: prev: {
  discord-canary = prev.discord-canary.override {withOpenASAR = true;};

  dummy = prev.hello;
}
