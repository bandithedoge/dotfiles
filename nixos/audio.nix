{...}: {
  # TODO: replace easyeffects with pipewire filter-chain
  services.pipewire = {
    enable = true;
    audio.enable = true;
    pulse.enable = true;
    jack.enable = true;

    lowLatency = {
      enable = true;
      quantum = 256;
    };

    alsa = {
      enable = true;
      support32Bit = true;
    };

    wireplumber = {
      enable = true;
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
