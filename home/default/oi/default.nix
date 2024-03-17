{pkgs ? import <nixpkgs> {}}:
pkgs.buildNimPackage {
  name = "oi";
  src = ./.;

  lockFile = ./lock.json;

  nativeBuildInputs = with pkgs; [
    makeWrapper
  ];

  postInstall = ''
    wrapProgram $out/bin/oi --prefix PATH : ${
      pkgs.lib.makeBinPath (with pkgs; [
        bash
        home-manager
        nix-output-monitor
        nvd
      ])
    }
  '';
}
