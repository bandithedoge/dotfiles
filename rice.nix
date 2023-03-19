rec {
  base00 = "#22262a";
  base01 = "#2B3035";
  base02 = "#383E45";
  base03 = "#868C96";
  base04 = "#c7c0b3";
  base05 = "#D6CFC1";
  base06 = "#E4DDCE";
  base07 = "#EFE7D8";
  base08 = "#f46a66";
  base09 = "#e0874b";
  base0A = "#e2b269";
  base0B = "#a0ba65";
  base0C = "#67c6c0";
  base0D = "#67aec9";
  base0E = "#cc7fbc";
  base0F = "#be93be";

  base10 = "#1A1D20";
  base11 = "#131517";
  base12 = base08;
  base13 = base0A;
  base14 = base0B;
  base15 = base0C;
  base16 = base0D;
  base17 = base0E;

  monoFont = "JetBrainsMono Nerd Font";
  uiFont = "Roboto Condensed";

  terminal = "kitty";
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
