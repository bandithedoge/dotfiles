{pkgs}: rec {
  base00 = "#2b3339";
  base01 = "#323c41";
  base02 = "#3a454a";
  base03 = "#868d80";
  base04 = "#d3c6aa";
  base05 = "#d3c6aa";
  base06 = "#e9e8d2";
  base07 = "#fff9e8";
  base08 = "#e67e80";
  base09 = "#e69875";
  base0A = "#dbbc7f";
  base0B = "#a7c080";
  base0C = "#83c092";
  base0D = "#7fbbb3";
  base0E = "#d699b6";
  base0F = "#93b259";

  base10 = "#232a2f";
  base11 = "#1e2327";
  base12 = base08;
  base13 = base0A;
  base14 = base0B;
  base15 = base0C;
  base16 = base0D;
  base17 = base0E;

  monoFont = "JetBrainsMono Nerd Font";
  uiFont = "Roboto Condensed";

  terminal = "st";
  wm = "river";
  menu = "rofi -show drun";

  wallpaper = ./wallpaper.jpg;
  wallpaperBlurred = pkgs.runCommand "blur" {} ''
    ${pkgs.imagemagick}/bin/magick ${./wallpaper.jpg} -gaussian-blur 0x12 -format png $out
  '';

  gtk = let
    color = pkgs.lib.removePrefix "#";
    src = pkgs.writeText "materia-rice" ''
      FG=${color base00}
      BG=${color base05}
      HDR_BG=${color base00}
      HDR_FG=${color base04}
      BTN_BG=${color base05}
      ACCENT_BG=${color base05}
      SEL_BG=${color base0F}
      TXT_BG=${color base05}

      ICONS_LIGHT_FOLDER=${color base05}
      ICONS_MEDIUM=${color base03}
      ICONS_DARK=${color base0F}
      ICONS_SYMBOLIC_ACTION=${color base05}
      ICONS_SYMBOLIC_PANEL=${color base04}

      TERMINAL_BACKGROUND=${color base00}
      TERMINAL_FOREGROUND=${color base05}
      TERMINAL_CURSOR=${color base0F}
      TERMINAL_COLOR0=${color base01}
      TERMINAL_COLOR1=${color base08}
      TERMINAL_COLOR2=${color base0B}
      TERMINAL_COLOR3=${color base09}
      TERMINAL_COLOR4=${color base0D}
      TERMINAL_COLOR5=${color base0E}
      TERMINAL_COLOR6=${color base0C}
      TERMINAL_COLOR7=${color base06}
      TERMINAL_COLOR8=${color base02}
      TERMINAL_COLOR9=${color base12}
      TERMINAL_COLOR10=${color base14}
      TERMINAL_COLOR11=${color base13}
      TERMINAL_COLOR12=${color base16}
      TERMINAL_COLOR13=${color base17}
      TERMINAL_COLOR14=${color base15}
      TERMINAL_COLOR15=${color base0F}
    '';
  in {
    theme = {
      name = "materia-rice";
      package = pkgs.oomoxPlugins.theme-materia.generate {
        inherit src;
        name = "materia-rice";
      };
    };
    iconTheme = {
      name = "suruplus-rice";
      package = pkgs.oomoxPlugins.icons-suruplus.generate {
        inherit src;
        name = "suruplus-rice";
      };
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
    '';
  };
}
