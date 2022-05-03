{
  config,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
    calf
    distrho
  ];

  services.pipewire = {
    enable = true;
    media-session.enable = false;
    wireplumber.enable = true;

    pulse.enable = true;
    jack.enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
  };

  musnix.enable = true;
}
