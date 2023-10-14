{pkgs}: let
  rice = builtins.fromJSON (builtins.readFile ./rice.json);
in
  rice
  // (with rice; rec {
    terminal = "kitty";
    wm = "river";
    menu = "rofi -show drun";

    wallpaper = ./wallpaper.jpg;
    wallpaperBlurred = pkgs.runCommand "blur" {} ''
      ${pkgs.imagemagick}/bin/magick ${wallpaper} -gaussian-blur 0x12 -format png $out
    '';

    gtk = let
      color = pkgs.lib.removePrefix "#";
      src = pkgs.writeText "materia-rice" (pkgs.lib.generators.toKeyValue {} {
        BG = color base00;
        FG = color base05;
        HDR_BG = color base02;
        HDR_FG = color base04;
        HDR_BTN_BG = color base01;
        HDR_BTN_FG = color base05;
        BTN_BG = color base02;
        BTN_FG = color base05;
        ACCENT_BG = color base0F;
        ACCENT_FG = color base00;
        SEL_BG = color base0F;
        TXT_BG = color base05;

        TERMINAL_BACKGROUND = color base00;
        TERMINAL_FOREGROUND = color base05;
        TERMINAL_CURSOR = color base0F;
        TERMINAL_COLOR0 = color base01;
        TERMINAL_COLOR1 = color base08;
        TERMINAL_COLOR2 = color base0B;
        TERMINAL_COLOR3 = color base09;
        TERMINAL_COLOR4 = color base0D;
        TERMINAL_COLOR5 = color base0E;
        TERMINAL_COLOR6 = color base0C;
        TERMINAL_COLOR7 = color base06;
        TERMINAL_COLOR8 = color base02;
        TERMINAL_COLOR9 = color base12;
        TERMINAL_COLOR10 = color base14;
        TERMINAL_COLOR11 = color base13;
        TERMINAL_COLOR12 = color base16;
        TERMINAL_COLOR13 = color base17;
        TERMINAL_COLOR14 = color base15;
        TERMINAL_COLOR15 = color base0F;

        MATERIA_COLOR_VARIANT = "dark";
        MATERIA_STYLE_COMPACT = "True";
        MATERIA_SURFACE = color base02;
        MATERIA_VIEW = color base01;
      });
    in {
      theme = {
        name = "Materia-Rice";
        package = pkgs.materia-theme.overrideAttrs (old: {
          nativeBuildInputs =
            old.nativeBuildInputs
            ++ (with pkgs; [
              bc
              optipng
              (runCommandLocal "rendersvg" {} ''
                mkdir -p $out/bin
                ln -s ${resvg}/bin/resvg $out/bin/rendersvg
              '')
            ]);
          patchPhase = ''
            sed -e '/handle-horz-.*/d' -e '/handle-vert-.*/d' \
              -i ./src/gtk-2.0/assets.txt

            export HOME="$NIX_BUILD_ROOT"

            patchShebangs .
            ./change_color.sh -o Materia-Rice ${src} -t $out/share/themes -i False
          '';
        });
      };
      iconTheme = {
        name = "Papirus-Dark";
        package = pkgs.papirus-icon-theme;
      };
      cursorTheme = {
        name = "phinger-cursors";
        package = pkgs.phinger-cursors;
        size = 16;
      };
    };

    def = rec {
      lua = ''
        base00 = "${base00}"
        base01 = "${base01}"
        base02 = "${base02}"
        base03 = "${base03}"
        base04 = "${base04}"
        base05 = "${base05}"
        base06 = "${base06}"
        base07 = "${base07}"
        base08 = "${base08}"
        base09 = "${base09}"
        base0A = "${base0A}"
        base0B = "${base0B}"
        base0C = "${base0C}"
        base0D = "${base0D}"
        base0E = "${base0E}"
        base0F = "${base0F}"

        base10 = "${base10}"
        base11 = "${base11}"
        base12 = "${base12}"
        base13 = "${base13}"
        base14 = "${base14}"
        base15 = "${base15}"
        base16 = "${base16}"
        base17 = "${base17}"

        monoFont = "${monoFont}"
        uiFont = "${uiFont}"

        terminal = "${terminal}"
        wm = "${wm}"
        menu = "${menu}"

        wallpaper = "${wallpaper}"
        wallpaperBlurred = "${wallpaperBlurred}"
      '';
      python = lua;
      scss = ''
        $base00: ${base00};
        $base01: ${base01};
        $base02: ${base02};
        $base03: ${base03};
        $base04: ${base04};
        $base05: ${base05};
        $base06: ${base06};
        $base07: ${base07};
        $base08: ${base08};
        $base09: ${base09};
        $base0A: ${base0A};
        $base0B: ${base0B};
        $base0C: ${base0C};
        $base0D: ${base0D};
        $base0E: ${base0E};
        $base0F: ${base0F};

        $base10: ${base10};
        $base11: ${base11};
        $base12: ${base12};
        $base13: ${base13};
        $base14: ${base14};
        $base15: ${base15};
        $base16: ${base16};
        $base17: ${base17};

        $monoFont: "${monoFont}";
        $uiFont: "${uiFont}";

        $wallpaper: "${wallpaper}";
        $wallpaperBlurred: "${wallpaperBlurred}";
      '';
      hypr = let
        c = color: "0xFF" + (pkgs.lib.removePrefix "#" color);
      in ''
        $base00 = ${c base00}
        $base01 = ${c base00}
        $base02 = ${c base00}
        $base03 = ${c base00}
        $base04 = ${c base00}
        $base05 = ${c base00}
        $base06 = ${c base00}
        $base07 = ${c base00}
        $base08 = ${c base00}
        $base09 = ${c base09}
        $base0A = ${c base0A}
        $base0B = ${c base0B}
        $base0C = ${c base0C}
        $base0D = ${c base0D}
        $base0E = ${c base0E}
        $base0F = ${c base0F}

        $base10 = ${c base10}
        $base11 = ${c base11}
        $base12 = ${c base12}
        $base13 = ${c base13}
        $base14 = ${c base14}
        $base15 = ${c base15}
        $base16 = ${c base16}
        $base17 = ${c base17}

        $monoFont = ${monoFont}
        $uiFont = ${uiFont}

        $terminal = ${terminal}
        $wm = ${wm}
        $menu = ${menu}

        $wallpaper = ${wallpaper}
        $wallpaperBlurred = ${wallpaperBlurred}
      '';
    };

    compileSCSS = path: let
      input = pkgs.writeText "input.scss" ''
        ${def.scss}
        ${builtins.readFile path}
      '';
    in
      pkgs.runCommand "output.css" {} ''
        ${pkgs.sassc}/bin/sassc ${input} > $out
      '';
  })
