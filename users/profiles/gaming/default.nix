{
  config,
  pkgs,
  ...
}: {
  home.packages = with pkgs; [
    bandithedoge.raze
    gzdoom
    prismlauncher
  ];
}
