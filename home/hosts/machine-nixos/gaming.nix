{ pkgs, ... }:
{
  home.packages = with pkgs; [
    bandithedoge.vkdoom
    doomrunner
    pcsx2
    prismlauncher
  ];

  xdg.dataFile."Steam/compatibilitytools.d/proton-ge".source =
    pkgs.bandithedoge.proton.ge.steamcompattool;

  services.flatpak = {
    remotes = [
      {
        name = "launcher.moe";
        location = "https://gol.launcher.moe/gol.launcher.moe.flatpakrepo";
      }
    ];
    packages = [
      {
        appId = "moe.launcher.an-anime-game-launcher";
        origin = "launcher.moe";
      }
      {
        appId = "moe.launcher.the-honkers-railway-launcher";
        origin = "launcher.moe";
      }
    ];
  };
}
