{pkgs, ...}: {
  # TODO: vfio
  environment.systemPackages = with pkgs; [
    virt-manager
    virtiofsd
  ];

  virtualisation = {
    libvirtd.enable = true;
    virtualbox.host.enable = true;
  };
}
