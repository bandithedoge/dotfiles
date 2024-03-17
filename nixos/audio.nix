{...}: {
  # TODO: replace easyeffects with pipewire filter-chain
  services.pipewire = {
    enable = true;
    audio.enable = true;
    pulse.enable = true;
    jack.enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    extraConfig = let
      rate = 48000;
      bufsize = 512;
    in {
      pipewire."99-ll" = {
        "context.properties" = {
          "default.clock.rate" = rate;
          "default.clock.quantum" = bufsize;
        };
      };
      pipewire-pulse."99-ll" = {
        "stream.properties" = {
          "node.latency" = "${builtins.toString bufsize}/${builtins.toString rate}";
          "resample.quality" = 1;
        };
      };
    };
  };

  musnix = {
    enable = true;
    das_watchdog.enable = true;
  };
}
