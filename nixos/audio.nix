{...}: {
  services.pipewire = {
    enable = true;
    audio.enable = true;
    pulse.enable = true;
    jack.enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
  };

  environment.etc = let
    rate = 48000;
    bufsize = 256;
  in {
    "pipewire/pipewire.conf.d/99-ll.conf".text = builtins.toJSON {
      "context.properties" = {
        "default.clock.rate" = rate;
        "default.clock.quantum" = bufsize;
      };
    };
    "pipewire/pipewire-pulse.conf.d/99-ll.conf".text = builtins.toJSON {
      "stream.properties" = {
        "node.latency" = "${builtins.toString bufsize}/${builtins.toString rate}";
        "resample.quality" = 1;
      };
    };
  };

  musnix.enable = true;
}
