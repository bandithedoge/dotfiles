{pkgs ? import <nixpkgs> {}}:
pkgs.nimPackages.buildNimPackage {
  name = "oi";
  src = ./.;

  nimBinOnly = true;

  nativeBuildInputs = with pkgs; [
    makeWrapper
  ];

  buildInputs = with pkgs.nimPackages; [
    docopt
  ];

  postInstall = ''
    wrapProgram $out/bin/oi --prefix PATH : ${
      pkgs.lib.makeBinPath (with pkgs; [
        bash
        home-manager
        nix-output-monitor
      ])
    }
  '';
}
