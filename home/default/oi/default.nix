{pkgs ? import <nixpkgs> {}}:
pkgs.buildNimPackage {
  name = "oi";
  src = ./.;

  nativeBuildInputs = with pkgs; [
    makeWrapper
  ];

  preBuild = let
    nimCfg = pkgs.lib.pipe ./lock.json [
      builtins.readFile
      builtins.fromJSON
      (builtins.getAttr "depends")
      (builtins.filter ({packages, ...}: !(builtins.any (pkg: builtins.elem pkg []) packages)))
      (map ({
        path,
        srcDir,
        ...
      }: ''path:"${builtins.storePath path}/${srcDir}"''))
      pkgs.lib.concatLines
      (pkgs.writeText "nim.cfg")
    ];
  in ''
    cp ${nimCfg} ./nim.cfg
  '';

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
