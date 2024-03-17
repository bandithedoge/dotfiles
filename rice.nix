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

    gtk = {
      theme = {
        name = "adw-gtk3-dark";
        package = pkgs.adw-gtk3;
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
      elisp = ''
        (defconst base00 "${base00}" "Default Background")
        (defconst base01 "${base01}" "Lighter Background (Used for status bars, line number and folding marks)")
        (defconst base02 "${base02}" "Selection Background")
        (defconst base03 "${base03}" "Comments, Invisibles, Line Highlighting")
        (defconst base04 "${base04}" "Dark Foreground (used for status bars)")
        (defconst base05 "${base05}" "Default Foreground, Caret, Delimiters, Operators")
        (defconst base06 "${base06}" "Light Foreground (Not often used)")
        (defconst base07 "${base07}" "Light Background (Not often used)")
        (defconst base08 "${base08}" "(Red) Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted")
        (defconst base09 "${base09}" "(Orange) Integers, Boolean, Constants, XML Attributes, Markup Link Url")
        (defconst base0A "${base0A}" "(Yellow) Classes, Markup Bold, Search Text Background")
        (defconst base0B "${base0B}" "(Green) Strings, Inherited Class, Markup Code, Diff Inserted")
        (defconst base0C "${base0C}" "(Cyan) Support, Regular Expressions, Escape Characters, Markup Quotes")
        (defconst base0D "${base0D}" "(Blue) Functions, Methods, Attribute IDs, Headings")
        (defconst base0E "${base0E}" "(Magenta) Keywords, Storage, Selector, Markup Italic, Diff Changed")
        (defconst base0F "${base0F}" "Deprecated, Opening/Closing Embedded Language Tags, e.g. <?php>")

        (defconst base10 "${base10}" "Darker Background")
        (defconst base11 "${base11}" "Darkest Background")
        (defconst base12 "${base12}" "Bright Red")
        (defconst base13 "${base13}" "Bright Yellow")
        (defconst base14 "${base14}" "Bright Green")
        (defconst base15 "${base15}" "Bright Cyan")
        (defconst base16 "${base16}" "Bright Blue")
        (defconst base17 "${base17}" "Bright Magenta")

        (defconst mono-font "${monoFont}" "monoFont")
        (defconst ui-font "${uiFont}" "uiFont")
      '';
    };

    compileSCSS = path: let
      input = pkgs.writeText "input.scss" ''
        ${def.scss}
        ${builtins.readFile path}
      '';
    in
      pkgs.runCommand "output.css" {} ''
        ${pkgs.dart-sass}/bin/sass ${input} > $out
      '';
  })
