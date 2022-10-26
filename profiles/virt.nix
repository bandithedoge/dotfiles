{
  config,
  pkgs,
  ...
}: {
  virtualisation = {
    libvirtd.enable = true;
    virtualbox.host = {
      enable = true;
    };
  };

  environment.systemPackages = with pkgs; [virt-manager];
}
