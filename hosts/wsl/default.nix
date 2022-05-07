{suites, config, pkgs, ...}: {
  imports = with suites; [base];
  wsl = {
    enable = true;
    automountPath = "/mnt";
    defaultUser = "nixos";
    startMenuLaunchers = true;
  };
}