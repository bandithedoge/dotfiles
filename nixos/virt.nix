{pkgs, ...}: {
  environment.systemPackages = with pkgs; [virt-manager virtiofsd];

  virtualisation = {
    libvirtd.enable = true;
    docker.enable = true;
  };
}
