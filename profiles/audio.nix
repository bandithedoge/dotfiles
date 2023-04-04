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
    samples = 512;
  in {
    "pipewire/pipewire.conf.d/99-ll.conf".text = builtins.toJSON {
      "context.properties" = {
        "link.max-buffers" = 16;
        "log.level" = 2;
        "default.clock.rate" = rate;
        "default.clock.quantum" = samples;
        "default.clock.min-quantum" = samples;
        "default.clock.max-quantum" = samples;
        "core.daemon" = true;
        "core.name" = "pipewire-0";
      };
      "context.modules" = [
        {
          name = "libpipewire-module-rtkit";
          args = {
            "nice.level" = -15;
            "rt.prio" = 88;
            "rt.time.soft" = 200000;
            "rt.time.hard" = 200000;
          };
          flags = ["ifexists" "nofail"];
        }
        {name = "libpipewire-module-protocol-native";}
        {name = "libpipewire-module-profiler";}
        {name = "libpipewire-module-spa-device-factory";}
        {name = "libpipewire-module-spa-node-factory";}
        {
          name = "libpipewire-module-portal";
          flags = ["ifexists" "nofail"];
        }
        {
          name = "libpipewire-module-access";
          args = {};
        }
        {name = "libpipewire-module-adapter";}
        {name = "libpipewire-module-link-factory";}
        {name = "libpipewire-module-session-manager";}
      ];
    };
    "pipewire/pipewire-pulse.conf.d/99-ll.conf".text = builtins.toJSON {
      "context.properties" = {
        "log.level" = 2;
      };
      "context.modules" = [
        {
          name = "libpipewire-module-rtkit";
          args = {
            "nice.level" = -15;
            "rt.prio" = 88;
            "rt.time.soft" = 200000;
            "rt.time.hard" = 200000;
          };
          flags = ["ifexists" "nofail"];
        }
        {name = "libpipewire-module-protocol-native";}
        {name = "libpipewire-module-adapter";}
      ];
      "stream.properties" = {
        "node.latency" = "${builtins.toString samples}/${builtins.toString rate}";
        "resample.quality" = 1;
      };
    };
  };

  musnix.enable = true;
}
