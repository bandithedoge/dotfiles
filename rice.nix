rec {
  base00 = "#303446";
  base01 = "#414559";
  base02 = "#51576d";
  base03 = "#838ba7";
  base04 = "#b5bfe2";
  base05 = "#c6d0f5";
  base06 = "#c6d0f5";
  base07 = "#c6d0f5";
  base08 = "#e78284";
  base09 = "#ef9f76";
  base0A = "#e5c890";
  base0B = "#a6d189";
  base0C = "#81c8be";
  base0D = "#8caaee";
  base0E = "#ca9ee6";
  base0F = "#babbf1";

  base10 = "#292c3c";
  base11 = "#232634";
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

  def = {
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
    '';
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
