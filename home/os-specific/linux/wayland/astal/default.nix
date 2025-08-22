{ pkgs, ... }:
{
  home.packages =
    with pkgs;
    let
      makeFennelBin =
        {
          name,
          src,
          libs ? [ ],
        }:
        let
          inherit (bandithedoge.luaPackages.astal-lua) lua;

          compiled = lua'.pkgs.toLuaModule (
            stdenvNoCC.mkDerivation {
              name = name + "-compiled";
              inherit src;

              nativeBuildInputs = [ fennel ];

              buildPhase = ''
                for f in $(find -name "*.fnl"); do
                  mkdir -p $out/share/lua/${lua'.luaversion}/${name}/$(dirname $f)
                  fennel --add-macro-path ${./macros.fnl} -c $f > $out/share/lua/${lua'.luaversion}/${name}/$(dirname $f)/$(basename $f .fnl).lua
                done
              '';
            }
          );

          lua' = lua.withPackages (_: [
            bandithedoge.luaPackages.astal-lua
            compiled
          ]);

          script = writeScript name ''
            #!${lua'}/bin/lua
            require("${name}")
          '';
        in
        stdenv.mkDerivation {
          inherit name src;

          nativeBuildInputs = [
            wrapGAppsHook3
            gobject-introspection
          ];

          buildInputs =
            with astal;
            [
              astal3
              io
            ]
            ++ libs;

          buildPhase = ''
            mkdir -p $out/bin
            cp ${script} $out/bin/$name
          '';
        };
    in
    [
      (makeFennelBin {
        name = "astal-bar";
        src = ./bar;
        libs = with pkgs.astal; [ niri ];
      })
    ];
}
