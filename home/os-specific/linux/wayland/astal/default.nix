{pkgs, ...}: let
  compileFennel = path: let
    macroPath = pkgs.runCommand "astal-fennel-macro-path" {} ''
      mkdir -p $out
      cp -r ${./config} $out/config
    '';
  in
    pkgs.runCommand "fennel-${path}" {
      nativeBuildInputs = with pkgs; [fennel dart-sass];
    } ''
      cd ${path}

      for f in $(find -name "*.fnl"); do
        mkdir -p $out/$(dirname $f)
        fennel --add-macro-path "${macroPath}/?.fnl" -c $f > $out/$(dirname $f)/$(basename $f ".fnl").lua
      done

      tmp=$(mktemp)
      cat << "EOF" > $tmp
      ${pkgs.rice.def.lua}
      EOF
      cat $out/init.lua >> $tmp
      mv $tmp $out/init.lua

      for f in $(find -name "*.scss"); do
        mkdir -p $out/$(dirname $f)
        tmp=$(mktemp)
        cat << "EOF" > $tmp
        ${pkgs.rice.def.scss}
      EOF
        cat $f >> $tmp
        sass $tmp > $out/$(dirname $f)/$(basename $f ".scss").css
      done
    '';
in {
  home.packages = with pkgs; [
    astal.io
    (astal.lib.mkLuaPackage {
      inherit pkgs;
      name = "astal-bar";
      src = compileFennel ./bar;
      extraPackages = with astal; [tray];
      extraLuaPackages = ps: with ps; [cjson];
    })
  ];
}
