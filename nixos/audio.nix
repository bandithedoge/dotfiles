{
  pkgs,
  lib,
  ...
}: {
  # TODO: replace easyeffects with pipewire filter-chain
  services.pipewire = let
    rate = 48000;
    bufsize = 64;
    qr = "${builtins.toString bufsize}/${builtins.toString rate}";
  in {
    enable = true;
    audio.enable = true;
    pulse.enable = true;
    jack.enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };

    extraConfig.pipewire."99-ll" = {
      context = {
        properties.default.clock.min-quantum = bufsize;
        modules = [
          {
            name = "libpipewire-module-rtkit";
            flags = ["ifexists" "nofail"];
            args = {
              nice.level = -15;
              rt = {
                prio = 88;
                time = {
                  soft = 200000;
                  hard = 200000;
                };
              };
            };
          }
          {
            name = "libpipewire-module-protocol-pulse";
            args = {
              server.address = ["unix:native"];
              pulse.min = {
                req = qr;
                quantum = qr;
                frag = qr;
              };
            };
          }
        ];
        stream.properties = {
          node.latency = qr;
          resample.quality = 1;
        };
      };
      "context.properties" = {
        "default.clock.rate" = rate;
        "default.clock.quantum" = bufsize;
      };
    };

    wireplumber = {
      enable = true;
      configPackages = [
        (pkgs.writeTextDir "share/wireplumber/wireplumber.conf.d/99-ll.conf" ''
          alsa_monitor.rules = {
            {
              matches = ${lib.generators.toLua {} [[["node.name" "matches" "alsa_output.*"]]]},
              apply_properties = ${lib.generators.toLua {} {
            "audio.format" = "S32LE";
            "audio.rate" = rate * 2;
            "api.alsa.period-size" = 2;
          }},
            }
          }
        '')
      ];
      extraConfig = {
        "51-disable-suspension" = {
          "monitor.alsa.rules" = [
            {
              matches = [
                {"node.name" = "~alsa_input.*";}
                {"node.name" = "~alsa_output.*";}
              ];
              actions.update-props = {
                "session.suspend-timeout-seconds" = 0;
              };
            }
          ];
        };
      };
    };
  };

  security.pam.loginLimits = [
    {
      domain = "@audio";
      type = "-";
      item = "rtprio";
      value = "99";
    }
    {
      domain = "@audio";
      type = "-";
      item = "memlock";
      value = "unlimited";
    }
  ];
}
